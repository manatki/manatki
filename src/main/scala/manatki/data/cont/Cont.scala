package manatki.data.cont

import alleycats.std.iterable._
import cats.Eval.{defer, later, now}
import cats.mtl.MonadState
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{Eval, Foldable, Monad, Monoid, StackSafeMonad}

trait Cont[R, +A] {
  def run(k: Eval[A] => Eval[R]): Eval[R]
  def runS(k: A => R): R = run(a => later(k(a.value))).value
}

trait ContS[R, +A] extends Cont[R, A] {
  def runStrict(k: A => R): R
  override def runS(k: A => R): R                  = runStrict(k)
  override def run(k: Eval[A] => Eval[R]): Eval[R] = now(runStrict(a => k(now(a)).value))
}

object Cont {
  type State[R, S, A] = Cont[S => Eval[R], A]

  implicit class ResettableOps[R](val c: Cont[R, R]) extends AnyVal {
    def reset: Eval[R] = Cont.reset(c)
    def resetS: R      = Cont.resetS(c)
  }

  def apply[R, A](f: ContS[R, A]): Cont[R, A] = f

  def callCCS[R, A, B](c: CallCCS[R, A, B]): Cont[R, A] = c

  def callCC[R, A, B](c: CallCC[R, A, B]): Cont[R, A] = c

  /** Reader / State like operation */
  def get[S, R]: State[R, S, S] = Cont(k => e => k(e)(e))

  /** state constructor */
  def state[S, R, B](r: S => (B, S)): State[R, S, B] =
    k => later(s => later(r(s)).flatMap { case (b, s1) => k(now(b)).flatMap(_(s1)) })

  def modify[S, R](f: S => S): State[R, S, Unit] =
    k => now(s => defer(k(Eval.now(())).flatMap(g => g(f(s)))))

  def put[S, R](s: S): State[R, S, Unit] =
    k => now(_ => defer(k(Eval.now(()))).flatMap(g => g(s)))

  def shift[A, R](c: Shift[R, A]): Cont[R, A]   = c
  def shiftS[A, R](c: ShiftS[R, A]): Cont[R, A] = c

  def reset[R](c: Cont[R, R]): Eval[R] = c.run(identity)
  def resetS[R](c: Cont[R, R]): R      = c.runS(identity)

  /** list effect constructor */
  def foldable[F[_]: Foldable, R: Monoid, A](x: F[A]): Cont[R, A] =
    f => x.foldMapM(a => f(now(a)))

  def range[R](start: Int, stop: Int)(implicit R: Monoid[R]): Cont[R, Int] =
    foldable(Iterable.range(start, stop))

  def guardC[R: Monoid](cond: Boolean): Cont[R, Unit] =
    Cont(f => if (cond) f(()) else Monoid.empty[R])

  implicit class MonoidOps[R, A](val c: Cont[R, A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit R: Monoid[R]): Cont[R, A] =
      c.flatMap(x => guardC[R](f(x)) as x)
  }

  implicit def monadStateInstance[R, S]: MonadState[State[R, S, ?], S] = new ContStateMonad

  implicit def monadInstance[R]: Monad[Cont[R, ?]] = new ContMonad[R]

  class ContMonad[R] extends StackSafeMonad[Cont[R, ?]] {
    override def flatMap[A, B](fa: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] =
      k => now(fa).flatMap(_.run(_.flatMap(a => f(a).run(k))))
    override def pure[A](x: A): Cont[R, A] = f => f(now(x))
  }

  abstract class CallCCS[R, A, B] extends ContS[R, A] {
    def ccs(k: A => Cont[R, B]): Cont[R, A]
    def runStrict(k: A => R): R = ccs(a => Cont(_ => k(a))).runS(k)
  }

  abstract class CallCC[R, A, B] extends Cont[R, A] {
    def cc(k: Eval[A] => Cont[R, B]): Cont[R, A]
    def run(k: Eval[A] => Eval[R]): Eval[R] = cc(a => _ => k(a)).run(k)
  }

  abstract class Shift[R, +A] extends Cont[R, A] {
    def shift(f: Eval[A] => Eval[R]): Cont[R, R]
    def run(k: Eval[A] => Eval[R]): Eval[R] = defer(reset(shift(k)))
  }

  abstract class ShiftS[R, +A] extends ContS[R, A] {
    def shiftS(f: A => R): Cont[R, R]
    def runStrict(k: A => R): R = resetS(shiftS(k))
  }

  class ContStateMonad[S, R] extends ContMonad[S => Eval[R]] with MonadState[State[R, S, ?], S] {
    val monad: Monad[State[R, S, ?]]          = this
    def get: State[R, S, S]                   = Cont.get
    def set(s: S): State[R, S, Unit]          = Cont.put(s)
    def inspect[A](f: S => A): State[R, S, A] = Cont.get[S, R].map(f)
    def modify(f: S => S): State[R, S, Unit]  = Cont.modify(f)
  }

}
