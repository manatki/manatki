package manatki.control
import cats.Eval

trait Delay[A] {
  def delay(a: => A): A
}

object Delay {
  private object anyForceDelay extends Delay[Any] {
    def delay(a: => Any): Any = a
  }
  private object anyEvalDelay extends Delay[Eval[Any]] {
    def delay(a: => Eval[Any]): Eval[Any] = Eval.defer(a)
  }

  def force[A]: Delay[A] = anyForceDelay.asInstanceOf[Delay[A]]
  implicit def evalDelay[A]: Delay[Eval[A]] = anyEvalDelay.asInstanceOf[Delay[Eval[A]]]
}
