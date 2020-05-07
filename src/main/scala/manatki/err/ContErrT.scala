package manatki.err

import cats.{Applicative, Functor, Monad}
import manatki.err.Ctx.{OfProc => CtxProc}
import manatki.err.Res.{Aux => ErrTo}
import tofu.syntax.handle._
import tofu.syntax.monadic._
import tofu.syntax.raise._
import tofu.{Handle, Raise}

trait ContErrT[F[+_], C, -E <: Res, +A] { self =>
  def run(ctx: CtxProc[C, F]): F[A]

  def local[C1](f: CtxProc[C1, F] => CtxProc[C, F]): ContErrT[F, C1, E, A] = ctx => self.run(f(ctx))

  def flatMap[E1 <: E, B](f: A => ContErrT[F, C, E1, B])(implicit F: Monad[F]): ContErrT[F, C, E1, B] =
    new ContErrT[F, C, E1, B] {
      def run(ctx: CtxProc[C, F]): F[B] = self.run(ctx).flatMap(f(_).run(ctx))

      override def flatMap[E2 <: E1, D](
          g: B => ContErrT[F, C, E2, D]
      )(implicit F: Monad[F]): ContErrT[F, C, E2, D] =
        self.flatMap(a => f(a).flatMap(g))
    }

  def map[B](f: A => B)(implicit F: Functor[F]): ContErrT[F, C, E, B] = self.run(_).map(f)

  def handle[A1 >: A](
      handler: ErrTo[E, A1]
  )(implicit FH: Handle[F, Throwable], F: Applicative[F]): ContErrT.Successful[F, C, A1] =
    ctx =>
      (self.run(ctx): F[A1]).recover[Throwable] {
        case r: Raisen[E @unchecked] => r.handle(handler)
      }

  def handleWith[A1 >: A](
      handler: ErrTo[E, F[A1]]
  )(implicit F: Monad[F], FH: Handle[F, Throwable]): ContErrT.Successful[F, C, A1] =
    ctx =>
      (self.run(ctx): F[A1]).recoverWith[Throwable] {
        case r: Raisen[E @unchecked] => r.handle(handler)
      }
}

object ContErrT extends ProcInstances{
  def pure[F[+_]: Applicative, A](a: A): Successful[F, Any, A] = new Successful[F, Any, A] {
    def run(ctx: CtxProc[Any, F]): F[A] = a.pure[F]

    override def flatMap[E1 <: Err, B](
        f: A => ContErrT[F, Any, E1, B]
    )(implicit F: Monad[F]): ContErrT[F, Any, E1, B] = f(a)
  }

  def lift[F[+_], A](fa: F[A]): Successful[F, Any, A] = _ => fa

  def fail[F[+_], E <: Err]: Fail[F, E] = new Fail[F, E](true)

  class Fail[F[+_], E <: Err](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(r: Raisen.Lam[E, Arb])(implicit F: Raise[F, Throwable]): ContErrT[F, Any, E, Nothing] =
      new Failed[F, E](r)
  }

  class Absolute[F[+_], -E <: Err, +A](val done: F[A]) extends ContErrT[F, Any, E, A] {
    def run(ctx: CtxProc[Any, F]): F[A] = done

    def as[C]: ContErrT[F, C, E, A] = this.asInstanceOf[ContErrT[F, C, E, A]]

    override def local[C1](f: CtxProc[C1, F] => CtxProc[Any, F]): ContErrT[F, C1, E, A] = this.as[C1]

    override def map[B](f: A => B)(implicit F: Functor[F]): ContErrT[F, Any, E, B] = new Absolute(done.map(f))

    override def handle[A1 >: A](
        handler: ErrTo[E, A1]
    )(implicit FH: Handle[F, Throwable], F: Applicative[F]): Successful[F, Any, A1] = {
      val recovered = (done: F[A1]).recover[Throwable] {
        case r: Raisen[E @unchecked] => r.handle(handler)
      }
      new Absolute[F, Err, A1](recovered) with Successful[F, Any, A1]
    }
  }

  class Failed[F[+_]: Raise[*[_], Throwable], E <: Err](r: Raisen[E])
      extends Absolute[F, E, Nothing](r.raise[F, Nothing]) {
    override def flatMap[E1 <: E, B](f: Nothing => ContErrT[F, Any, E1, B])(
        implicit F: Monad[F]
    ): ContErrT[F, Any, E1, B] = super.flatMap(f)

    override def map[B](f: Nothing => B)(implicit F: Functor[F]): ContErrT[F, Any, E, B] = this

    override def handle[A1](
        handler: ErrTo[E, A1]
    )(implicit FH: Handle[F, Throwable], F: Applicative[F]): Successful[F, Any, A1] =
      pure(r.handle(handler))

    override def handleWith[A1 >: Nothing](
        handler: ErrTo[E, F[A1]]
    )(implicit F: Monad[F], FH: Handle[F, Throwable]): Successful[F, Any, A1] =
      lift(r.handle(handler))
  }

  trait Successful[F[+_], C, +A] extends ContErrT[F, C, Err, A] {
    override def handle[A1 >: A](
        handler: ErrTo[Err, A1]
    )(implicit FH: Handle[F, Throwable], F: Applicative[F]): Successful[F, C, A1] = this

    override def handleWith[A1 >: A](
        handler: ErrTo[Err, F[A1]]
    )(implicit F: Monad[F], FH: Handle[F, Throwable]): Successful[F, C, A1] = this
  }
}

trait ProcInstances{
//  implicit def monadError[F[+_], C, E <: Err, A]
}
