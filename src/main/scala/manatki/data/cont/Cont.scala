package manatki.data.cont

import Cont.{FlatMap, Pure}
import cats.mtl.MonadState
import cats.{Monad, StackSafeMonad}
import tofu.Raise

sealed trait Cont[R, A] {
  def map[B](f: A => B): Cont[R, B]              = flatMap(a => Cont.Pure(f(a)))
  def flatMap[B](f: A => Cont[R, B]): Cont[R, B] = FlatMap[R, A, B](this, f)
  def run(f: A => R): R                          = map(f).evalue
}

object Cont extends ContoInstances {
  type State[R, S, A] = Cont[S => Ev[R], A]
  type Ev[X]          = Cont[X, X]

  def shift[R, A](cn: Shift[R, A]): Cont[R, A] = cn

  case class Pure[R, A](a: A)                                     extends Cont[R, A]
  case class FlatMap[R, A, B](fa: Cont[R, A], f: A => Cont[R, B]) extends Cont[R, B]
  trait Shift[R, A] extends Cont[R, A] {
    def cont(kc: A => Ev[R]): Ev[R]
  }
  case class Reset[R, A](c: Ev[A]) extends Cont[R, A]

  implicit class EvOps[A](private val x: Ev[A]) extends AnyVal {
    def emap[B](f: A => B): Ev[B]         = Reset[B, A](x).map(f)
    def eflatMap[B](f: A => Ev[B]): Ev[B] = Reset[B, A](x).flatMap(f)
    def evalue: A                         = loop(x)(ResetStack.id)
  }

  import ResetStack._

  def loop[R, U](conto: Ev[R])(stack: ResetStack[R, U]): U =
    conto match {
      case Pure(a) =>
        stack match {
          case e: End[R, U]  => e.to(a)
          case Step(f, tail) => loop(f(a))(tail)
        }

      case u: Shift[R, R] => loop(u.cont(a => Pure(a)))(stack)
      case Reset(c)       => loop(c)(stack)
      case fm: FlatMap[R, x, R] =>
        type X = x
        fm.fa match {
          case Pure(b)               => loop(fm.f(b))(stack)
          case Reset(c)              => loop(c)(Step(fm.f, stack))
          case fm2: FlatMap[R, y, X] => loop(FlatMap[R, y, R](fm2.fa, y => FlatMap[R, x, R](fm2.f(y), fm.f)))(stack)
          case u: Shift[R, X]        => loop(u.cont(x => fm.f(x)))(stack)
        }
    }

  def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
    shift(k => f(a => shift(_ => k(a))).flatMap(k))

  def contState[S, R, A](f: ((A, S) => Ev[R], S) => Ev[R]): State[R, S, A] =
    shift(k => Reset(Pure(s => f((a, s) => k(a).eflatMap(_(s)), s))))

  def get[S, R]: State[R, S, S] = contState((k, s) => k(s, s))
  def state[S, R, B](r: S => (B, S)): State[R, S, B] =
    contState((k, s) => r(s) match { case (b, s1) => k(b, s1) })

  def set[S, R, B](s: S): State[R, S, Unit]         = contState((k, _) => k((), s))
  def update[S, R, B](f: S => S): State[R, S, Unit] = contState((k, s) => k((), f(s)))

  def raiseError[R, S, A](err: R): State[R, S, A] = contState((_, s) => Pure(err))

  class ContoMonad[R] extends StackSafeMonad[Cont[R, *]] {
    def pure[A](x: A): Cont[R, A]                                     = Pure(x)
    def flatMap[A, B](fa: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] = fa.flatMap(f)
  }

  class ContoStateMonad[S, R]
      extends ContoMonad[S => Cont[R, R]] with MonadState[State[R, S, *], S] with Raise[State[R, S, *], R] {
    val monad: Monad[State[R, S, *]]          = this
    def get: State[R, S, S]                   = Cont.get
    def set(s: S): State[R, S, Unit]          = Cont.set(s)
    def inspect[A](f: S => A): State[R, S, A] = Cont.get.map(f)
    def modify(f: S => S): State[R, S, Unit]  = Cont.update(f)
    def raise[A](err: R): State[R, S, A]      = raiseError(err)
  }

}
trait ContoInstances { self: Cont.type =>
  implicit def contoStateMonad[S, R]: MonadState[State[R, S, *], S] = new ContoStateMonad
  implicit def contoMonad[R]: Monad[Cont[R, *]]                     = new ContoMonad
}

sealed trait ResetStack[A, R]

object ResetStack {
  private val anyEnd: End[Any, Any] = x => x
  def id[A]: End[A, A]              = anyEnd.asInstanceOf[End[A, A]]

  trait End[A, R] extends ResetStack[A, R] {
    def to(a: A): R
    def apply(a: A): Cont[R, R] = Pure(to(a))
  }

  final case class Step[A, B, R](head: A => Cont[B, B], tail: ResetStack[B, R]) extends ResetStack[A, R]
}
