package manatki.data.cont

import alleycats.std.iterable._
import cats.Eval.{defer, later, now}
import cats.mtl.MonadState
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{Eval, Foldable, Monad, Monoid, StackSafeMonad}

trait ContE[R, +A] {
  def run(k: Eval[A] => Eval[R]): Eval[R]
  def runS(k: A => R): R = run(a => later(k(a.value))).value
}

trait ContS[R, +A] extends ContE[R, A] {
  def runStrict(k: A => R): R
  override def runS(k: A => R): R                  = runStrict(k)
  override def run(k: Eval[A] => Eval[R]): Eval[R] = now(runStrict(a => k(now(a)).value))
}

object ContE {
  type State[R, S, A] = ContE[S => Eval[R], A]

  implicit class ResettableOps[R](val c: ContE[R, R]) extends AnyVal {
    def reset: Eval[R] = ContE.reset(c)
    def resetS: R      = ContE.resetS(c)
  }

  def apply[R, A](f: ContS[R, A]): ContE[R, A] = f

  def callCCS[R, A, B](c: CallCCS[R, A, B]): ContE[R, A] = c

  def callCC[R, A, B](c: CallCC[R, A, B]): ContE[R, A] = c

  /** Reader / State like operation */
  def get[S, R]: State[R, S, S] = ContE(k => s => k(s)(s))

  /** state constructor */
  def state[S, R, B](r: S => (B, S)): State[R, S, B] =
    k => later(s => later(r(s)).flatMap { case (b, s1) => k(now(b)).flatMap(_(s1)) })

  def modify[S, R](f: S => S): State[R, S, Unit] =
    k => now(s => defer(k(Eval.now(())).flatMap(g => g(f(s)))))

  def put[S, R](s: S): State[R, S, Unit] =
    k => now(_ => defer(k(Eval.now(()))).flatMap(g => g(s)))

  def shift[A, R](c: Shift[R, A]): ContE[R, A]   = c
  def shiftS[A, R](c: ShiftS[R, A]): ContE[R, A] = c

  def reset[R](c: ContE[R, R]): Eval[R] = c.run(identity)
  def resetS[R](c: ContE[R, R]): R      = c.runS(identity)

  /** list effect constructor */
  def foldable[F[_]: Foldable, R: Monoid, A](x: F[A]): ContE[R, A] =
    f => x.foldMapM(a => f(now(a)))

  def range[R](start: Int, stop: Int)(implicit R: Monoid[R]): ContE[R, Int] =
    foldable(Iterable.range(start, stop))

  def guardC[R: Monoid](cond: Boolean): ContE[R, Unit] =
    ContE(f => if (cond) f(()) else Monoid.empty[R])

  implicit class MonoidOps[R, A](val c: ContE[R, A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit R: Monoid[R]): ContE[R, A] =
      c.flatMap(x => guardC[R](f(x)) as x)
  }

  implicit def monadStateInstance[R, S]: MonadState[State[R, S, *], S] = new ContStateMonad

  implicit def monadInstance[R]: Monad[ContE[R, *]] = new ContMonad[R]

  class ContMonad[R] extends StackSafeMonad[ContE[R, *]] {
    override def flatMap[A, B](fa: ContE[R, A])(f: A => ContE[R, B]): ContE[R, B] =
      k => now(fa).flatMap(_.run(_.flatMap(a => f(a).run(k))))
    override def pure[A](x: A): ContE[R, A] = f => f(now(x))
  }

  abstract class CallCCS[R, A, B] extends ContS[R, A] {
    def ccs(k: A => ContE[R, B]): ContE[R, A]
    def runStrict(k: A => R): R = ccs(a => ContE(_ => k(a))).runS(k)
  }

  abstract class CallCC[R, A, B] extends ContE[R, A] {
    def cc(k: Eval[A] => ContE[R, B]): ContE[R, A]
    def run(k: Eval[A] => Eval[R]): Eval[R] = cc(a => _ => k(a)).run(k)
  }

  abstract class Shift[R, +A] extends ContE[R, A] {
    def shift(f: Eval[A] => Eval[R]): ContE[R, R]
    def run(k: Eval[A] => Eval[R]): Eval[R] = defer(reset(shift(k)))
  }

  abstract class ShiftS[R, +A] extends ContS[R, A] {
    def shiftS(f: A => R): ContE[R, R]
    def runStrict(k: A => R): R = resetS(shiftS(k))
  }

  class ContStateMonad[S, R] extends ContMonad[S => Eval[R]] with MonadState[State[R, S, *], S] {
    val monad: Monad[State[R, S, *]]          = this
    def get: State[R, S, S]                   = ContE.get
    def set(s: S): State[R, S, Unit]          = ContE.put(s)
    def inspect[A](f: S => A): State[R, S, A] = ContE.get[S, R].map(f)
    def modify(f: S => S): State[R, S, Unit]  = ContE.modify(f)
  }

}
