package manatki.err
import manatki.err.Res.Aux

trait Err {
  type Result
}

trait OnSuccess[-A] extends Err {
  def success(a: A): Result
}

object Res {
  private object err extends Err
  type Aux[+E, A] = E { type Result = A }
  def success[A]: Aux[Err, A] = err.asInstanceOf[Aux[Err, A]]
}

trait Opt extends Err {
  def none: Result
}

trait Raisen[-E] extends Throwable {
  def handle[A](x: Res.Aux[E, A]): A
}

object Raisen {
  abstract class Lam[E <: Res, R] extends Raisen[E] {
    def lamApply(e: Res.Aux[E, R]): R

    def handle[A](x: Res.Aux[E, A]): A = lamApply(x.asInstanceOf[Res.Aux[E, R]]).asInstanceOf[A]
  }
}

final class AsRes[E] private {
  type Result
}

object AsRes {
  type Aux[E, R] = AsRes[E]
  private[this] val instance = new AsRes[Any]

}
