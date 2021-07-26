package manatki.poc

import tofu.logging.Loggable
import doobie.util.Put
import tofu.logging.LoggedValue
import tofu.logging.LogRenderer
import scala.{specialized => sp}
import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.util.fragment.Fragment
import tofu.logging.Logging
import tofu.syntax.logging._
import tofu.doobie.log.LogHandlerF
import doobie.util.pos.Pos
import cats.effect.IOApp

import cats.data.NonEmptyList
import cats.effect.{ContextShift, Effect, ExitCode, Sync}
import cats.instances.string._
import cats.tagless.syntax.functorK._
import cats.{Apply, FlatMap, Monad}
import derevo.derive
import doobie._
import doobie.implicits._
import doobie.util.log.LogHandler
import tofu.common.Console
import tofu.doobie.LiftConnectionIO
import tofu.doobie.log.{EmbeddableLogHandler, LogHandlerF}
import tofu.doobie.transactor.Txr
import tofu.higherKind.Mid
import tofu.higherKind.derived.representableK
import tofu.lift.UnliftIO
import tofu.syntax.console._
import tofu.syntax.context._
import tofu.syntax.monadic._
import tofu.{WithContext, WithLocal, WithRun}
import cats.data.ReaderT
import cats.effect.IO
import tofu.logging.Logs
import tofu.logging.derivation.loggable
import tofu.Delay
import TDoobie._

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
}
object Putain {
  implicit def put[A: Put]: Put[Putain[A]] = Put[A].contramap(_.value)
}

class TofuSqlPart(val fragment: Fragment) extends AnyVal {
  def toFrag: SingleFragment[Nothing] = SingleFragment(fragment)
}

object TofuSqlPart {
  def apply[A](frag: SingleFragment[A]): TofuSqlPart                       = TofuSqlPart(frag.fr)
  implicit def fromPut[A: Loggable: Put](a: A): TofuSqlPart                = TofuSqlPart(Putain(a))
  implicit def fromPutOption[A: Loggable: Put](oa: Option[A]): TofuSqlPart = TofuSqlPart(oa.map(Putain(_)))
  implicit def fromFragment(fr: Fragment): TofuSqlPart                     = TofuSqlPart(fr)
}

object TDoobie {
  implicit class TofuSQLInterpolator(private val ctx: StringContext) extends AnyVal {
    def tfsql(parts: TofuSqlPart*)(implicit pos: Pos): Fragment =
      doobie.syntax.string.toSqlInterpolator(ctx).sql(parts.map(_.toFrag): _*)
  }

  def tofuLogHandler[F[_]: Logging](level: Logging.Level): LogHandlerF[F] = { evt =>
    val args = evt.args.collect { case lv: LoggedValue => lv }
    info"executing query ${evt.sql} with ${AccumLoggable(args)}"
  }
}

// Simple context
@derive(loggable)
final case class Ctx(traceId: String)

// Model
final case class Person(id: Long, name: String, deptId: Long)
final case class Dept(id: Long, name: String)
// create table department(id numeric primary key, name varchar);
// create table person(id numeric primary key, name varchar, dept_id numeric references department(id));

// Person SQL algebra
@derive(representableK)
trait PersonSql[F[_]] {
  def create(person: Person): F[Unit]
  def read(id: Long): F[Option[Person]]
}

object PersonSql {
  def make[DB[_]: Monad: LiftConnectionIO: Logging: EmbeddableLogHandler]: PersonSql[DB] = {
    val aspects = new LoggingMid[DB]
    val impl    = EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
    aspects attach impl
  }

  final class Impl(implicit lh: LogHandler) extends PersonSql[ConnectionIO] {
    def create(p: Person): ConnectionIO[Unit]        =
      tfsql"insert into person values(${p.id}, ${p.name}, ${p.deptId})".update.run.void
    def read(id: Long): ConnectionIO[Option[Person]] =
      tfsql"select id, name, dept_id from person where id = $id"
        .query[Person]
        .option
  }

  final class LoggingMid[DB[_]: Apply: Logging] extends PersonSql[Mid[DB, *]] {
    def create(person: Person): Mid[DB, Unit]   = info"create person" *> _
    def read(id: Long): Mid[DB, Option[Person]] = info"read person" *> _
  }
}

// Department SQL algebra
@derive(representableK)
trait DeptSql[F[_]] {
  def create(dept: Dept): F[Unit]
  def read(id: Long): F[Option[Dept]]
}

object DeptSql {
  def make[DB[_]: Monad: LiftConnectionIO: Logging: EmbeddableLogHandler]: DeptSql[DB] = {
    val aspects = new LoggingMid[DB]
    val impl    = EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
    aspects attach impl
  }

  final class Impl(implicit lh: LogHandler) extends DeptSql[ConnectionIO] {
    def create(d: Dept): ConnectionIO[Unit]        =
      tfsql"insert into department values(${d.id}, ${d.name})".update.run.void
    def read(id: Long): ConnectionIO[Option[Dept]] =
      tfsql"select id, name from department where id = $id"
        .query[Dept]
        .option
  }

  final class LoggingMid[DB[_]: Apply: Logging] extends DeptSql[Mid[DB, *]] {
    def create(dept: Dept): Mid[DB, Unit]     = info"create department" *> _
    def read(id: Long): Mid[DB, Option[Dept]] = info"read departmTasknt" *> _
  }
}

// Storage algebra encapsulates database transactional logic
@derive(representableK)
trait PersonStorage[F[_]] {
  def store(person: Person, dept: Dept): F[Unit]
}

object PersonStorage {
  def make[F[_]: Apply: Logging, DB[_]: Monad: Txr[F, *[_]]](
      persSql: PersonSql[DB],
      deptSql: DeptSql[DB]
  ): PersonStorage[F] = {
    val aspects = new LoggingMid[F]
    val impl    = new Impl[DB](persSql, deptSql): PersonStorage[DB]
    val tx      = Txr[F, DB].trans
    aspects attach impl.mapK(tx)
  }

  final class Impl[DB[_]: Monad](persSql: PersonSql[DB], deptSql: DeptSql[DB]) extends PersonStorage[DB] {
    def store(person: Person, dept: Dept): DB[Unit] =
      deptSql.create(dept) >> persSql.create(person)
  }

  final class LoggingMid[F[_]: Apply: Logging] extends PersonStorage[Mid[F, *]] {
    def store(person: Person, dept: Dept): Mid[F, Unit] = info"store dept & person" *> _
  }
}

object TofuDoobieExample extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    runF[IO, ReaderT[IO, Ctx, *]].as(ExitCode.Success)

  def runF[I[_]: Effect: ContextShift, F[_]: Sync: UnliftIO](implicit WR: WithRun[F, I, Ctx]): I[Unit] = {
    // Simplified wiring below
    implicit val loggingF = Logs.contextual[F, Ctx].byName("example")

    val transactor   = Transactor.fromDriverManager[I](
      driver = "org.postgresql.Driver",
      url = "jdbc:postgresql://localhost:5432/test",
      user = "postgres",
      pass = "secret"
    )
    implicit val txr = Txr.continuational(transactor.mapK(WR.liftF))

    def initStorage[
        DB[_]: Txr[F, *[_]]: Delay: Monad: Console: LiftConnectionIO: WithLocal[*[_], Ctx]: UnliftIO
    ]: PersonStorage[F] = {
      implicit val loggingDB = Logs.contextual[DB, Ctx].byName("example")

      implicit val elh = EmbeddableLogHandler.sync(tofuLogHandler[DB](Logging.Info))

      val personSql = PersonSql.make[DB]
      val deptSql   = DeptSql.make[DB]

      PersonStorage.make(personSql, deptSql)
    }

    val storage = initStorage
    val program = storage.store(Person(13L, "Alex", 42L), Dept(42L, "Marketing"))
    val launch  = WR.runContext(program)(Ctx("715a-562a-4da5-a6e0"))
    launch
  }
}
