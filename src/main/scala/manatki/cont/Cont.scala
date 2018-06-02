package manatki.cont

import alleycats.std.iterable._
import cats.Eval.{later, now}
import cats.syntax.foldable._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Eval, Foldable, Monad, Monoid, StackSafeMonad}


trait Cont[R, +A] {
  def run(k: Eval[A] => Eval[R]): Eval[R]
  def runS(k: A => R): R = run(a => later(k(a.value))).value
}

trait ContS[R, +A] extends Cont[R, A] {
  def runStrict(k: A => R): R
  override def runS(k: A => R): R = runStrict(k)
  override def run(k: Eval[A] => Eval[R]): Eval[R] = now(runStrict(a => k(now(a)).value))
}

object Cont {
  type State[R, S, A] = Cont[S => Eval[(R, S)], A]

  implicit class ResettableOps[R](val c: Cont[R, R]) extends AnyVal {
    def reset: Eval[R] = c.run(identity)
    def resetS: R = c.runS(identity)
  }

  def apply[R, A](f: (A => R) => R): Cont[R, A] = (g => f(g)): ContS[R, A]

  def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
    k => f(a => _ => k(now(a))).run(k)

  /** Reader / State like operation */
  def get[E, R]: Cont[E => R, E] = Cont(k => e => k(e)(e))

  /** state constructor */
  def state[S, R, B](r: S => (B, S)): State[R, S, B] = k => later {
    s => later(r(s)).flatMap { case (b, s1) => k(now(b)).flatMap(_ (s1)) }
  }

  def modify[S, R](f: S => S): State[R, S, Unit] = state(s => ((), f(s)))

  def put[S, R](s: S): State[R, S, Unit] = state(_ => ((), s))


  def shift[A, R](f: (Eval[A] => Eval[R]) => Cont[R, R]): Cont[R, A] = k => f(k).reset
  def shiftS[A, R](f: (A => R) => Cont[R, R]): ContS[R, A] = k => f(k).resetS

  /** list effect constructor */
  def foldable[F[_] : Foldable, R: Monoid, A](x: F[A]): Cont[R, A] =
    f => x.foldMapM(a => f(now(a)))

  def range[R](start: Int, stop: Int)(implicit R: Monoid[R]): Cont[R, Int] =
    foldable(Iterable.range(start, stop))

  def guardC[R: Monoid](cond: Boolean): Cont[R, Unit] =
    Cont(f => if (cond) f(()) else Monoid.empty[R])


  implicit def instance[R]: Monad[Cont[R, ?]] = new StackSafeMonad[Cont[R, ?]] {
    override def flatMap[A, B](fa: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] =
      k => now(fa).flatMap(_.run(_.flatMap(a => f(a).run(k))))
    override def pure[A](x: A): Cont[R, A] = f => f(now(x))
  }

  implicit class MonoidOps[R, A](val c: Cont[R, A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit R: Monoid[R]): Cont[R, A] =
      c.flatMap(x => guardC[R](f(x)) as x)
  }
}