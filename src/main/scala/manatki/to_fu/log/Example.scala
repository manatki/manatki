package manatki.to_fu.log
import tofu.syntax.logRenderer._
import tofu.logging.Loggable
import cats.syntax.show._
import cats.syntax.monoid._
import tofu.logging.DictLoggable
import tofu.logging.LogRenderer
import tofu.logging.LogTree
import cats.Show

sealed trait X
case class A(a: Int, b: String)                extends X
case class B(c: List[String], d: Option[Long]) extends X

object X {
  implicit object xLoggable extends DictLoggable[X] {
    def fields[I, V, R, S](a: X, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      a match {
        case A(a, b) =>
          i.field("x-type", "A") |+|
            i.field("x-a", a) |+|
            i.field("x-b", b)
        case B(c, d) =>
          i.field("x-type", "B") |+|
            i.field("x-c", c) |+|
            i.field("x-d", d)
      }

    def logShow(a: X): String =
      a match {
        case A(a, b) => show"A($a, $b)"
        case B(c, d) => show"B(${c.show}, $d)"
      }
  }
}

object Example extends App {
  println(LogTree[X](A(1, "lol")))
  println(LogTree[X](B(List("lol", "kek"), None)))
  println(LogTree[X](B(List("lol", "kek"), Some(2))))
}
