package manatki.instances

import tofu.WithRun
import tofu.WithProvide
import tofu.WithContext
import cats.Functor
import tofu.syntax.monadic._
import cats.Monad

class DerivedWithRun[F[_]: Monad, G[_], C](implicit
    FP: WithProvide[F, G, C],
    FC: WithContext[F, C]
) extends WithRun[F, G, C] {
  override def runContext[A](fa: F[A])(ctx: C): G[A] = FP.runContext(fa)(ctx)

  override def functor: Functor[F] = FC.functor

  override def context: F[C] = FC.context

  override def lift[A](fa: G[A]): F[A] = FP.lift(fa)

  override def local[A](fa: F[A])(project: C => C): F[A] =
    FC.context.flatMap(c => lift(runContext(fa)(project(c))))
}
