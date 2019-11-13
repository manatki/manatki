package manatki.data.cont

import alleycats.std.iterable._
import cats.data.AndThen
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{Foldable, Monad, Monoid, StackSafeMonad}

trait ContX[X, +A] {
  def run(k: A => X): X
  def flatMap[B](f: A => ContX[X, B]): ContX[X, B] =
    k => run(AndThen(f).andThen(_.run(k)))

}

object ContX {
  type State[X, S, A] = ContX[S => (X, S), A]

//  implicit class XesettableOps[X](val c: Cont[X, X]) extends AnyVal {
//    def reset: Eval[X] = Cont.reset(c)
//    def resetS: X      = Cont.resetS(c)
//  }

  def pure[X, A](a: A): ContX[X, A] = _(a)

  def callCC[X, A, B](f: (A => ContX[X, B]) => ContX[X, A]): ContX[X, A] =
    k => f(a => _ => k(a)).run(k)

  /** Xeader / State like operation */
  def get[E, X]: ContX[E => X, E] = k => e => k(e)(e)

  /** state constructor */
  def state[S, X, B](r: S => (B, S)): State[X, S, B] =
    k => s => r(s) match { case (b, s1) => k(b)(s1) }

  def modify[S, X](f: S => S): State[X, S, Unit] = state(s => ((), f(s)))

  def put[S, X](s: S): State[X, S, Unit] = state(_ => ((), s))

  def shift[A, X](f: (A => X) => ContX[X, X]): ContX[X, A] = k => reset(f(k))

  def reset[X](c: ContX[X, X]): X = c.run(identity)

  /** list effect constructor */
  def foldable[F[_]: Foldable, X: Monoid, A](x: F[A]): ContX[X, A] =
    f => x.foldMap(a => f(a))

  def range[X](start: Int, stop: Int)(implicit X: Monoid[X]): ContX[X, Int] =
    foldable(Iterable.range(start, stop))

  def guardC[X: Monoid](cond: Boolean): ContX[X, Unit] =
    f => if (cond) f(()) else Monoid.empty[X]

  implicit def instance[X]: Monad[ContX[X, *]] = new StackSafeMonad[ContX[X, *]] {
    override def flatMap[A, B](fa: ContX[X, A])(f: A => ContX[X, B]): ContX[X, B] =
      k => fa.run(AndThen(f).andThen(_.run(k)))
    override def pure[A](x: A): ContX[X, A] = _(x)
  }

  implicit class MonoidOps[X, A](val c: ContX[X, A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit X: Monoid[X]): ContX[X, A] =
      c.flatMap(x => guardC[X](f(x)) as x)
  }
}
