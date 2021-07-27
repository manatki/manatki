package manatki.poc

import scala.{specialized => sp}

import cats.data.{NonEmptyList, ReaderT}
import cats.effect.{ContextShift, Effect, ExitCode, IO, IOApp, Sync}
import cats.instances.string._
import cats.tagless.syntax.functorK._
import cats.{Apply, FlatMap, Id, Monad}
import derevo.derive
import doobie._
import doobie.implicits._
import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.util.Put
import doobie.util.fragment.Fragment
import doobie.util.log.LogHandler
import doobie.util.pos.Pos
import tofu.common.Console
import tofu.doobie.log.{EmbeddableLogHandler, LogHandlerF}
import tofu.doobie.transactor.Txr
import tofu.doobie.{ConnectionCIO, LiftConnectionIO}
import tofu.higherKind.Mid
import tofu.higherKind.derived.representableK
import tofu.lift.UnliftIO
import tofu.logging.derivation.{loggable, loggingMidTry}
import tofu.logging.{LogRenderer, Loggable, LoggedValue, Logging, LoggingCompanion, Logs}
import tofu.syntax.console._
import tofu.syntax.context._
import tofu.syntax.logging._
import tofu.syntax.monadic._
import tofu.{BracketThrow, Delay, Tries, WithContext, WithLocal, WithRun}

import TDoobie._
import doobie.util.log

class AccumLoggable(values: Seq[LoggedValue]) extends LoggedValue {
  override def shortName: String = "sql arguments"

  override def toString = values.mkString("(", ", ", ")")

  def logFields[I, V, @specialized R, @specialized M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = {
    values.foldLeft(r.noop(input)) { (res, p) => r.combine(res, p.logFields(input)) }
  }
}

object AccumLoggable {
  def apply(values: Seq[LoggedValue]): LoggedValue = new AccumLoggable(values)
}

case class Putain[A](value: A)(implicit val log: Loggable[A]) extends LoggedValue {
  def logFields[I, V, @sp(Unit) R, @sp M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = log.fields(value, input)
  override def putValue[I, V, R, S](v: V)(implicit r: LogRenderer[I, V, R, S]): S           = log.putValue(value, v)

  override def toString = log.logShow(value)
}
object Putain {
  implicit def put[A: Put]: Put[Putain[A]] = Put[A].contramap(_.value)
}

class TofuSqlPart(val fragment: Fragment) extends AnyVal {
  def toFrag: SingleFragment[Nothing] = SingleFragment(fragment)
}

object TofuSqlPart {
  def apply[A](frag: SingleFragment[A]): TofuSqlPart                       = new TofuSqlPart(frag.fr)
  implicit def fromPut[A: Loggable: Put](a: A): TofuSqlPart                = TofuSqlPart(Putain(a))
  implicit def fromPutOption[A: Loggable: Put](oa: Option[A]): TofuSqlPart = TofuSqlPart(oa.map(Putain(_)))
  implicit def fromFragment(fr: Fragment): TofuSqlPart                     = TofuSqlPart(fr)
}

object TDoobie {
  implicit class TofuSQLInterpolator(private val ctx: StringContext) extends AnyVal {
    def tfsql(parts: TofuSqlPart*)(implicit pos: Pos): Fragment =
      doobie.syntax.string.toSqlInterpolator(ctx).sql(parts.map(_.toFrag): _*)
  }

  def tofuLogHandler[F[_]: Logs.Universal](level: Logging.Level): LogHandlerF[F] = {
    implicit val logs                        = Logs[Id, F].named["doobie"]
    def args(evt: log.LogEvent): LoggedValue = AccumLoggable(evt.args.collect { case lv: LoggedValue => lv })

    {
      case evt: log.Success           =>
        info"executed query ${evt.sql} with ${args(evt)}"
      case evt: log.ExecFailure       =>
        errorCause"failed query ${evt.sql} with ${args(evt)}" (evt.failure)
      case evt: log.ProcessingFailure =>
        errorCause"failed process query ${evt.sql} with ${args(evt)}" (evt.failure)
    }
  }
}

// Simple context
@derive(loggable)
final case class Ctx(traceId: String)

// Model
@derive(loggable)
final case class Person(id: Long, name: String, deptId: Long)

@derive(loggable)
final case class Dept(id: Long, name: String)
// create table department(id numeric primary key, name varchar);
// create table person(id numeric primary key, name varchar, dept_id numeric references department(id));

// Person SQL algebra
@derive(representableK, loggingMidTry)
trait PersonSql[F[_]] {
  def init: F[Unit]
  def create(person: Person): F[Unit]
  def read(id: Long): F[Option[Person]]
}

object PersonSql extends LoggingCompanion[PersonSql] {
  def make[DB[_]: Monad: LiftConnectionIO: EmbeddableLogHandler]: PersonSql[DB] = {
    EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
  }

  final class Impl(implicit lh: LogHandler) extends PersonSql[ConnectionIO] {
    def init: ConnectionIO[Unit]                     =
      tfsql"create table if not exists person(id int8, name varchar(50), dept_id int8)".update.run.void
    def create(p: Person): ConnectionIO[Unit]        =
      tfsql"insert into person values(${p.id}, ${p.name}, ${p.deptId})".update.run.void
    def read(id: Long): ConnectionIO[Option[Person]] =
      tfsql"select id, name, dept_id from person where id = $id"
        .query[Person]
        .option
  }
}

// Department SQL algebra
@derive(representableK, loggingMidTry)
trait DeptSql[F[_]] {
  def init: F[Unit]
  def create(dept: Dept): F[Unit]
  def read(id: Long): F[Option[Dept]]
}

object DeptSql extends LoggingCompanion[DeptSql] {
  def make[DB[_]: Monad: LiftConnectionIO: EmbeddableLogHandler]: DeptSql[DB] = {
    EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
  }

  final class Impl(implicit lh: LogHandler) extends DeptSql[ConnectionIO] {

    def init: ConnectionIO[Unit]                   =
      tfsql"create table if not exists department(id int8, name varchar(50))".update.run.void
    def create(d: Dept): ConnectionIO[Unit]        =
      tfsql"insert into department valuez(${d.id}, ${d.name})".update.run.void
    def read(id: Long): ConnectionIO[Option[Dept]] =
      tfsql"select id, name from department where id = $id"
        .query[Dept]
        .option
  }
}

// Storage algebra encapsulates database transactional logic
@derive(representableK, loggingMidTry)
trait PersonStorage[F[_]] {
  def init: F[Unit]
  def store(person: Person, dept: Dept): F[Unit]
}

object PersonStorage extends LoggingCompanion[PersonStorage] {
  def make[F[_]: Apply, DB[_]: Monad: Txr[F, *[_]]](
      persSql: PersonSql[DB],
      deptSql: DeptSql[DB]
  ): PersonStorage[F] = {
    val impl = new Impl[DB](persSql, deptSql): PersonStorage[DB]
    val tx   = Txr[F, DB].trans
    impl.mapK(tx)
  }

  final class Impl[DB[_]: Monad](persSql: PersonSql[DB], deptSql: DeptSql[DB]) extends PersonStorage[DB] {
    def init: DB[Unit]                              = deptSql.init >> persSql.init
    def store(person: Person, dept: Dept): DB[Unit] =
      deptSql.create(dept) >> persSql.create(person)
  }
}

object TofuDoobieExample extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    runF[IO, ReaderT[IO, Ctx, *]].as(ExitCode.Success)

  def runF[I[_]: Effect: ContextShift, F[_]: Sync: UnliftIO](implicit WR: WithRun[F, I, Ctx]): I[Unit] = {
    // Simplified wiring below
    implicit val loggingF = Logs.contextual[F, Ctx]

    val transactor   = Transactor.fromDriverManager[I](
      // driver = "org.h2.Driver",
      // url = "jdbc:h2:./test",
      driver = "org.postgresql.Driver",
      url = "jdbc:postgresql://localhost:5432/postgres",
      user = "postgres",
      pass = "secret"
    )
    implicit val txr = Txr.continuational(transactor.mapK(WR.liftF))

    def initStorage[
        DB[_]: Tries: Txr[F, *[_]]: Delay: Monad: Console: LiftConnectionIO: WithLocal[*[_], Ctx]: UnliftIO
    ]: PersonStorage[F] = {
      implicit val loggingDB = Logs.contextual[DB, Ctx]

      implicit val elh = EmbeddableLogHandler.sync(tofuLogHandler[DB](Logging.Info))

      val personSql = PersonSql.make[DB].attachErrLogs
      val deptSql   = DeptSql.make[DB].attachErrLogs

      PersonStorage.make[F, DB](personSql, deptSql).attachErrLogs
    }

    val storage = initStorage[ConnectionCIO[F, *]]
    val program = storage.init >> storage.store(Person(13L, "Alex", 42L), Dept(42L, "Marketing"))
    val launch  = WR.runContext(program)(Ctx("715a-562a-4da5-a6e0"))
    launch
  }
}
