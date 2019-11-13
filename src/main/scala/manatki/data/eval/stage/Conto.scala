package manatki.data.eval.stage
import cats.{Monad, StackSafeMonad}
import cats.mtl.MonadState
import manatki.data.eval.stage.Conto.FlatMap

sealed trait Conto[R, A] {
  def map[B](f: A => B): Conto[R, B]               = flatMap(a => Conto.Pure(f(a)))
  def flatMap[B](f: A => Conto[R, B]): Conto[R, B] = FlatMap[R, A, B](this, f)
}

object Conto {
  type State[R, S, A] = Conto[S => Conto[R, R], A]

  def shift[R, A](cn: Shift[R, A]): Conto[R, A] = cn

  case class Pure[R, A](a: A)                                       extends Conto[R, A]
  case class FlatMap[R, A, B](fa: Conto[R, A], f: A => Conto[R, B]) extends Conto[R, B]
  trait Shift[R, A] extends Conto[R, A] {
    def cont(kc: A => Conto[R, R]): Conto[R, R]
  }
  case class Reset[R, A](c: Conto[A, A]) extends Conto[R, A]

  def loop[R, U](conto: Conto[R, R])(stack: LoopStack[R, U]): U =
    conto match {
      case Pure(a) =>
        stack match {
          case e: End[R, U]      => e.to(a)
          case LoopStep(f, tail) => loop(f(a))(tail)
        }

      case u: Shift[R, R] => loop(u.cont(a => Pure(a)))(stack)
      case Reset(c)       => loop(c)(stack)
      case fm: FlatMap[R, x, R] =>
        type X = x
        fm.fa match {
          case Pure(b)               => loop(fm.f(b))(stack)
          case Reset(c)              => loop(c)(LoopStep(fm.f, stack))
          case fm2: FlatMap[R, y, X] => loop(FlatMap[R, y, R](fm2.fa, y => FlatMap[R, x, R](fm2.f(y), fm.f)))(stack)
          case u: Shift[R, X]        => loop(u.cont(x => fm.f(x)))(stack)
        }
    }

  sealed trait LoopStack[A, R]

  trait End[A, R] extends LoopStack[A, R] {
    def to(a: A): R
    def apply(a: A): Conto[R, R] = Pure(to(a))
  }

  final case class LoopStep[A, B, R](head: A => Conto[B, B], tail: LoopStack[B, R]) extends LoopStack[A, R]

  object End {
    private val anyEnd: End[Any, Any] = x => x
    def id[A]: End[A, A]              = anyEnd.asInstanceOf[End[A, A]]
  }

  class ContoMonad[R] extends StackSafeMonad[Conto[R, *]] {
    def pure[A](x: A): Conto[R, A]                                       = Pure(x)
    def flatMap[A, B](fa: Conto[R, A])(f: A => Conto[R, B]): Conto[R, B] = fa.flatMap(f)
  }

  class ContoStateMonad[I, R] extends ContoMonad[I => R] with MonadState[Conto[I => R, *], I] {
    val monad: Monad[Conto[I => R, *]]          = this
    def get: Conto[I => R, I]                   = ???
    def set(s: I): Conto[I => R, Unit]          = ???
    def inspect[A](f: I => A): Conto[I => R, A] = ???
    def modify(f: I => I): Conto[I => R, Unit]  = ???
  }

}
