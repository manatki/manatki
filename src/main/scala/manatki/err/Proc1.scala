package manatki.err

import cats.Functor
import manatki.err.Ctx.{OfProc => CtxProc}
import manatki.err.Err.{Aux => ErrTo}
import tofu.syntax.monadic._

trait Proc1[F[+_], -C <: Ctx, -E <: Err, +A] { self =>
  def run[C1 <: C, R](
      ctx: CtxProc[C1, F],
      h: ErrTo[E, R],
      k: A => R
  ): F[R]

  def handle[A1 >: A](handler: ErrTo[E, A1])(implicit F: Functor[F]): Proc1[F, C, Err, A1] =
    new Proc1[F, C, Err, A1] {
       def run[C1 <: C, R](ctx: CtxProc[C1, F], h: ErrTo[Err, R], k: A1 => R): F[R] =
        self.run[C1, A1](ctx, handler, identity).map(k)
    }
}
