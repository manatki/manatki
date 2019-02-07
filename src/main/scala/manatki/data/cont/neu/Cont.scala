package manatki.data.cont.neu

import alleycats.std.iterable._
import cats.syntax.foldable._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monoid._
import cats.syntax.applicative._
import cats.{Foldable, Monad, Monoid, StackSafeMonad}
import Monoid.empty

trait Cont[R, +A] {
  def run(k: A => R): R
}

object Cont {
  type State[R, S, A] = Cont[S => (R, S), A]

//  implicit class ResettableOps[R](val c: Cont[R, R]) extends AnyVal {
//    def reset: Eval[R] = Cont.reset(c)
//    def resetS: R      = Cont.resetS(c)
//  }

  def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
    k => f(a => _ => k(a)).run(k)

  /** Reader / State like operation */
  def get[E, R]: Cont[E => R, E] = k => e => k(e)(e)

  /** state constructor */
  def state[S, R, B](r: S => (B, S)): State[R, S, B] =
    k => s => r(s) match { case (b, s1) => k(b)(s1) }

  def modify[S, R](f: S => S): State[R, S, Unit] = state(s => ((), f(s)))

  def put[S, R](s: S): State[R, S, Unit] = state(_ => ((), s))

  def shift[A, R](f: (A => R) => Cont[R, R]): Cont[R, A] = k => reset(f(k))

  def reset[R](c: Cont[R, R]): R = c.run(identity)

  /** list effect constructor */
  def foldable[F[_]: Foldable, R: Monoid, A](x: F[A]): Cont[R, A] =
    f => x.foldMap(a => f(a))

  def range[R](start: Int, stop: Int)(implicit R: Monoid[R]): Cont[R, Int] =
    foldable(Iterable.range(start, stop))

  def guardC[R: Monoid](cond: Boolean): Cont[R, Unit] =
    f => if (cond) f(()) else Monoid.empty[R]

  implicit def instance[R]: Monad[Cont[R, ?]] = new StackSafeMonad[Cont[R, ?]] {
    override def flatMap[A, B](fa: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] =
      k => fa.run(a => f(a).run(k))
    override def pure[A](x: A): Cont[R, A] = _(x)
  }

  implicit class MonoidOps[R, A](val c: Cont[R, A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit R: Monoid[R]): Cont[R, A] =
      c.flatMap(x => guardC[R](f(x)) as x)
  }

}
