package manatki.poc

import tofu.logging.Loggable
import doobie.util.Put
import tofu.logging.LoggedValue
import tofu.logging.LogRenderer
import scala.{specialized => sp}
import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.util.fragment.Fragment

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
    def tfsql(parts: TofuSqlPart*): Fragment =
      doobie.syntax.string.toSqlInterpolator(ctx).sql(parts.map(_.toFrag): _*)
  }
}
