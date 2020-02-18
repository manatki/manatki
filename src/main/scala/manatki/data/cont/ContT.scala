package manatki.data.cont

import ContT.{FlatMap, Pure}
import cats.mtl.MonadState
import cats.{Functor, Monad, StackSafeMonad}
import tofu.Raise

sealed trait ContT[+F[_], R, A] {
  def map[B](f: A => B): ContT[F, R, B]                  = flatMap(a => ContT.Pure(f(a)))
  def flatMap[B](f: A => ContT[F, R, B]): ContT[F, R, B] = FlatMap[R, A, B](this, f)
  def run(f: A => R): R                                  = map(f).evalue
}

object ContT extends ContToInstances {
  type StateF[S, A]   = S => Ev[A]
  type State[R, S, A] = ContT[StateF, R, A]
  type EvF[+F[_], X]  = ContT[F, X, X]
  type Ev[X]          = EvF[Nothing, X]

  def shift[F[_], R, A](cn: Shift[F, R, A]): ContT[F, R, A] = cn

  case class Pure[R, A](a: A)                                                    extends ContT[Nothing, R, A]
  case class FlatMap[+F[_], R, A, B](fa: ContT[F, R, A], f: A => ContT[F, R, B]) extends ContT[F, R, B]
  trait Shift[F, R, A] extends ContT[F, R, A] {
    def cont(kc: A => EvF[F, R]): EvF[F, R]
  }
  case class Reset[F[_], R, A](c: EvF[F, A]) extends ContT[F, R, A]

//  implicit class EvOps[A](private val x: Ev[A]) extends AnyVal {
//    def emap[B](f: A => B): Ev[B]         = Reset[B, A](x).map(f)
//    def eflatMap[B](f: A => Ev[B]): Ev[B] = Reset[B, A](x).flatMap(f)
//    def evalue: A                         = loop(x)(ResetStack.id)
//  }

  import ResetStack._

  def step[F[_]: Functor, R, U](conto: EvF[F, R])(stack: ResetStack[F, R, U]): Either[F[NextStep[F, _, U]], U] =
    conto match {
      case Pure(a) =>
        stack match {
          case e: End[R, U]  => Right(e.to(a))
          case Step(f, tail) => step(f(a))(tail)
        }

      case u: Shift[R, R] => step(u.cont(a => Pure(a)))(stack)
      case Reset(c)       => step(c)(stack)
      case fm: FlatMap[R, x, R] =>
        type X = x
        fm.fa match {
          case Pure(b)               => step(fm.f(b))(stack)
          case Reset(c)              => step(c)(Step(fm.f, stack))
          case fm2: FlatMap[R, y, X] => step(FlatMap[R, y, R](fm2.fa, y => FlatMap[R, x, R](fm2.f(y), fm.f)))(stack)
          case u: Shift[R, X]        => step(u.cont(x => fm.f(x)))(stack)
        }
    }
//
//  def callCC[R, A, B](f: (A => ContT[R, B]) => ContT[R, A]): ContT[R, A] =
//    shift(k => f(a => shift(_ => k(a))).flatMap(k))
//
//  def contState[S, R, A](f: ((A, S) => Ev[R], S) => Ev[R]): State[R, S, A] =
//    shift(k => Reset(Pure(s => f((a, s) => k(a).eflatMap(_(s)), s))))
//
//  def get[S, R]: State[R, S, S] = contState((k, s) => k(s, s))
//  def state[S, R, B](r: S => (B, S)): State[R, S, B] =
//    contState((k, s) => r(s) match { case (b, s1) => k(b, s1) })
//
//  def set[S, R, B](s: S): State[R, S, Unit]         = contState((k, _) => k((), s))
//  def update[S, R, B](f: S => S): State[R, S, Unit] = contState((k, s) => k((), f(s)))
//
//  def raiseError[R, S, A](err: R): State[R, S, A] = contState((_, s) => Pure(err))
//
//  class ContToMonad[R] extends StackSafeMonad[ContT[R, *]] {
//    def pure[A](x: A): ContT[R, A]                                       = Pure(x)
//    def flatMap[A, B](fa: ContT[R, A])(f: A => ContT[R, B]): ContT[R, B] = fa.flatMap(f)
//  }
//
//  class ContToStateMonad[S, R]
//      extends ContToMonad[S => ContT[R, R]] with MonadState[State[R, S, *], S] with Raise[State[R, S, *], R] {
//    val monad: Monad[State[R, S, *]]          = this
//    def get: State[R, S, S]                   = ContT.get
//    def set(s: S): State[R, S, Unit]          = ContT.set(s)
//    def inspect[A](f: S => A): State[R, S, A] = ContT.get.map(f)
//    def modify(f: S => S): State[R, S, Unit]  = ContT.update(f)
//    def raise[A](err: R): State[R, S, A]      = raiseError(err)
//  }

  final case class NextStep[+F[_], R, U](cont: EvF[F, R], stack: ResetStack[F, R, U])


}
//trait ContToInstances { self: ContT.type =>
//  implicit def contoStateMonad[S, R]: MonadState[State[R, S, *], S] = new ContToStateMonad
//  implicit def contoMonad[R]: Monad[ContT[R, *]]                    = new ContToMonad
//}

sealed trait ResetStack[+F[_], A, R]

object ResetStack {
  private val anyEnd: End[Any, Any] = x => x
  def id[A]: End[A, A]              = anyEnd.asInstanceOf[End[A, A]]

  trait End[A, R] extends ResetStack[Nothing, A, R] {
    def to(a: A): R
    def apply(a: A): ContT[Nothing, R, R] = Pure(to(a))
  }

  final case class Step[F[_], A, B, R](head: A => ContT[F, B, B], tail: ResetStack[F, B, R]) extends ResetStack[F, A, R]
}

