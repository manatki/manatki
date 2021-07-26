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

  def logHandler[F[_]: Logging](level: Logging.Level): LogHandlerF[F] = { evt =>
    val args = evt.args.collect { case lv: LoggedValue => lv }
    info"executing query ${evt.sql} with ${AccumLoggable(args)}"
  }
}
