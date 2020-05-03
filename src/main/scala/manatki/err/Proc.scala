package manatki.err

import cats.{Applicative, FlatMap, Functor, Monad}
import manatki.err.Ctx.{OfProc => CtxProc}
import manatki.err.Err.{Aux => ErrTo}
import manatki.err.Proc.SuccessfulProc
import tofu.syntax.monadic._

trait Proc[F[+_], -C <: Ctx, -E <: Err, +A] { self =>
  protected[manatki] def runWith[C1 <: C](ctx: CtxProc[C1, F]): F[Attempt[E, A]]

  def run[C1 <: C, E1 <: E, R](
      ctx: CtxProc[C1, F],
      h: ErrTo[E1 with OnSuccess[A], R]
  )(implicit F: Functor[F]): F[R] = runWith[C1](ctx).map {
    case r: Raisen[E1] @unchecked => r.handle(h)
    case a: A @unchecked          => h.success(a)
  }

  def flatMap[C1 <: C, E1 <: E, B](f: A => Proc[F, C1, E1, B])(implicit F: Monad[F]): Proc[F, C1, E1, B] =
    new Proc[F, C1, E1, B] {

      protected[manatki] def runWith[C2 <: C1](ctx: CtxProc[C2, F]): F[Attempt[E1, B]] =
        self.runWith(ctx).flatMap {
          case r: Raisen[E1] @unchecked => F.pure(r.attempt)
          case a: A @unchecked          => f(a).runWith(ctx)
        }

      override def flatMap[C2 <: C1, E2 <: E1, D](
          g: B => Proc[F, C2, E2, D]
      )(implicit F: Monad[F]): Proc[F, C2, E2, D] =
        self.flatMap(a => f(a).flatMap(g))
    }

  def map[B](f: A => B)(implicit F: Functor[F]): Proc[F, C, E, B] = new Proc[F, C, E, B] {
    def runWith[C1 <: C](ctx: CtxProc[C1, F]): F[Attempt[E, B]] =
      self.runWith(ctx).map(_.atmap(f))
  }

  def handle[A1 >: A](handler: ErrTo[E, A1])(implicit F: Functor[F]): Proc[F, C, Err, A1] =
    new SuccessfulProc[F, C, A1] with (Attempt[E, A1] => Attempt[Err, A1]) {
      def apply(x: Attempt[E, A1]): Attempt[Err, A1] = Attempt.success(x.recov[A1](handler))

      def runWith[C1 <: C](ctx: CtxProc[C1, F]): F[Attempt[Err, A1]] =
        self.runWith(ctx).map(this)
    }

  def handleWith[A1 >: A](handler: ErrTo[E, F[A1]])(implicit F: Monad[F]): Proc[F, C, Err, A1] =
    new SuccessfulProc[F, C, A1] with (Attempt[E, A1] => F[Attempt[Err, A1]]) {
      def apply(x: Attempt[E, A1]): F[Attempt[Err, A1]] =
        Attempt.subst[F, A1](x.atmap(F.pure[A1]).recov[F[A1]](handler))

      def runWith[C1 <: C](ctx: CtxProc[C1, F]): F[Attempt[Err, A1]] =
        self.runWith(ctx).flatMap(this)
    }
}

object Proc {
  def pure[F[+_]: Applicative, A](a: A): Proc[F, Ctx, Err, A] = new SuccessfulProc[F, Ctx, A] {
    def runWith[C1 <: Ctx](ctx: CtxProc[C1, F]): F[Attempt[Err, A]] = Attempt.success(a).pure[F]

    override def flatMap[C1 <: Ctx, E1 <: Err, B](
        f: A => Proc[F, C1, E1, B]
    )(implicit F: Monad[F]): Proc[F, C1, E1, B] =
      f(a)

    override def run[C1 <: Ctx, E1 <: Err, R](ctx: CtxProc[C1, F], h: ErrTo[E1 with OnSuccess[A], R])(
        implicit F: Functor[F]
    ): F[R] = h.success(a).pure[F]
  }

  def fail[F[+_], E <: Err]: Fail[F, E] = new Fail[F, E](true)

  class Fail[F[+_], E <: Err](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(r: Raisen.Lam[E, Arb])(implicit F: Applicative[F]): Proc[F, Ctx, E, Nothing] = new FailedProc[F, E](r)
  }

  class FailedProc[F[+_]: Applicative, E <: Err](r: Raisen[E]) extends Proc[F, Ctx, E, Nothing] {
    private[this] val failed                                                               = r.attempt.pure[F]
    protected[manatki] def runWith[C1 <: Ctx](ctx: CtxProc[C1, F]): F[Attempt[E, Nothing]] = failed

    override def flatMap[C1 <: Ctx, E1 <: E, B](f: Nothing => Proc[F, C1, E1, B])(
        implicit F: Monad[F]
    ): Proc[F, C1, E1, B] = super.flatMap(f)

    override def run[C1 <: Ctx, E1 <: E, R](ctx: CtxProc[C1, F], h: ErrTo[E1 with OnSuccess[Nothing], R])(
        implicit F: Functor[F]
    ): F[R] =
      r.handle(h).pure[F]

    override def map[B](f: Nothing => B)(implicit F: Functor[F]): Proc[F, Ctx, E, B] = this

    override def handle[A1 >: Nothing](handler: ErrTo[E, A1])(implicit F: Functor[F]): Proc[F, Ctx, Err, A1] =
      pure(r.handle(handler))
  }

  trait SuccessfulProc[F[+_], C <: Ctx, A] extends Proc[F, C, Err, A] {
    override def handle[A1 >: A](handler: ErrTo[Err, A1])(implicit F: Functor[F]): Proc[F, C, Err, A1] = this

    override def handleWith[A1 >: A](handler: ErrTo[Err, F[A1]])(implicit F: Monad[F]): Proc[F, C, Err, A1] = this
  }
}
