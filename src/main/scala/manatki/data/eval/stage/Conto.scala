package manatki.data.eval.stage
import cats.{Monad, StackSafeMonad}
import cats.mtl.MonadState
import manatki.data.eval.stage.Conto.FlatMap

sealed trait Conto[R, A] {
  def map[B](f: A => B): Conto[R, B]               = flatMap(a => Conto.Pure(f(a)))
  def flatMap[B](f: A => Conto[R, B]): Conto[R, B] = FlatMap[R, A, B](this, f)
  def run(f: A => R): R                            = map(f).evalue
}

object Conto extends ContoInstances {
  type State[R, S, A] = Conto[S => Ev[R], A]
  type Ev[X]          = Conto[X, X]

  def shift[R, A](cn: Shift[R, A]): Conto[R, A] = cn

  case class Pure[R, A](a: A)                                       extends Conto[R, A]
  case class FlatMap[R, A, B](fa: Conto[R, A], f: A => Conto[R, B]) extends Conto[R, B]
  trait Shift[R, A] extends Conto[R, A] {
    def cont(kc: A => Ev[R]): Ev[R]
  }
  case class Reset[R, A](c: Ev[A]) extends Conto[R, A]

  implicit class EvOps[A](private val x: Ev[A]) extends AnyVal {
    def emap[B](f: A => B): Ev[B]         = Reset[B, A](x).map(f)
    def eflatMap[B](f: A => Ev[B]): Ev[B] = Reset[B, A](x).flatMap(f)
    def evalue: A                         = loop(x)(End.id)
  }

  def loop[R, U](conto: Ev[R])(stack: LoopStack[R, U]): U =
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

  def get[S, R]: State[R, S, S] = shift(k => Reset(Pure(s => k(s).eflatMap(_(s)))))
  def state[S, R, B](r: S => (B, S)): State[R, S, B] =
    shift(k => Reset(Pure(s => r(s) match { case (b, s1) => k(b).eflatMap(_(s1)) })))

  def set[S, R, B](s: S): State[R, S, Unit]         = shift(k => Reset(Pure(_ => k(()).eflatMap(_(s)))))
  def update[S, R, B](f: S => S): State[R, S, Unit] = shift(k => Reset(Pure(s => k(()).eflatMap(_(f(s))))))

  class ContoMonad[R] extends StackSafeMonad[Conto[R, *]] {
    def pure[A](x: A): Conto[R, A]                                       = Pure(x)
    def flatMap[A, B](fa: Conto[R, A])(f: A => Conto[R, B]): Conto[R, B] = fa.flatMap(f)
  }

  class ContoStateMonad[S, R] extends ContoMonad[S => Conto[R, R]] with MonadState[State[R, S, *], S] {
    val monad: Monad[State[R, S, *]]          = this
    def get: State[R, S, S]                   = Conto.get
    def set(s: S): State[R, S, Unit]          = Conto.set(s)
    def inspect[A](f: S => A): State[R, S, A] = Conto.get.map(f)
    def modify(f: S => S): State[R, S, Unit]  = Conto.update(f)
  }

}
trait ContoInstances { self: Conto.type =>
  implicit def contoStateMonad[S, R]: MonadState[State[R, S, *], S] = new ContoStateMonad
  implicit def contoMonad[R]: Monad[Conto[R, *]]                    = new ContoMonad
}
