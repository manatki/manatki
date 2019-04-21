package manatki.data.cont.stage

import alleycats.std.iterable._
import cats.data.AndThen
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{Foldable, Monad, Monoid, StackSafeMonad}

trait ContR[X, R, +A] {
  def run(k: (A, R) => X, r: R): X
  def runA(k: A => X, r: R) = run((a, _) => k(a), r)
  def flatMap[B](f: A => ContR[X, R, B]): ContR[X, R, B] =
    (k, r) => runA(AndThen(f).andThen(_.run(k, r)), r)

}

object ContR {

  def pure[X, R, A](a: A): ContR[X, R, A] = _(a)

  def callCC[X, A, B](f: (A => ContR[X, B]) => ContR[X, A]): ContR[X, A] =
    k => f(a => _ => k(a)).run(k)

  /** Xeader / State like operation */
  def get[E, X]: ContR[E => X, E] = k => e => k(e)(e)

  /** state constructor */
  def state[S, X, B](r: S => (B, S)): State[X, S, B] =
    k => s => r(s) match { case (b, s1) => k(b)(s1) }

  def modify[S, X](f: S => S): State[X, S, Unit] = state(s => ((), f(s)))

  def put[S, X](s: S): State[X, S, Unit] = state(_ => ((), s))

  def shift[A, X](f: (A => X) => ContR[X, X]): ContR[X, A] = k => reset(f(k))

  def reset[X](c: ContR[X, X]): X = c.run(identity)

  /** list effect constructor */
  def foldable[F[_]: Foldable, X: Monoid, A](x: F[A]): ContR[X, A] =
    f => x.foldMap(a => f(a))

  def range[X](start: Int, stop: Int)(implicit X: Monoid[X]): ContR[X, Int] =
    foldable(Iterable.range(start, stop))

  def guardC[X: Monoid](cond: Boolean): ContR[X, Unit] =
    f => if (cond) f(()) else Monoid.empty[X]

  implicit def instance[X]: Monad[ContR[X, ?]] = new StackSafeMonad[ContR[X, ?]] {
    override def flatMap[A, B](fa: ContR[X, A])(f: A => ContR[X, B]): ContR[X, B] =
      k => fa.run(AndThen(f).andThen(_.run(k)))
    override def pure[A](x: A): ContR[X, A] = _(x)
  }

  implicit class MonoidOps[X, A](val c: ContR[X, A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit X: Monoid[X]): ContR[X, A] =
      c.flatMap(x => guardC[X](f(x)) as x)
  }
}
