package manatki.syntax

import cats.Monad
import cats.data.OptionT
import cats.effect.Resource
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._
import monocle._
import tofu.BracketThrow

object ref {

  /**
    * strategy optimistic modification of state
    * it cat try to get value of type R from state
    * or mutate the state using X
    */
  type OptimMod[S, R, X] = POptional[S, S, R, X]

  implicit class RefManatkiOps[F[_], A](private val self: Ref[F, A]) extends AnyVal {

    /** tries to avoid running `init` if state contains suitable value */
    def optimisticModify[B, X, R](lens: OptimMod[A, R, X])(init: => F[X])(f: X => R)(implicit F: Monad[F]): F[R] =
      OptionT(self.get.map(lens.getOption)).getOrElseF(init.flatMap(x =>
        self.modify(a =>
          lens.getOption(a) match {
            case Some(r) => (a, r)
            case None    => (lens.set(x)(a), f(x))
        })))

    /** tries to avoid initializing resource if state contains suitable value */
    def optimisticModifyRes[B, X, R](lens: OptimMod[A, R, X])(init: => Resource[F, X])(f: X => R)(
        implicit F: BracketThrow[F]): F[R] =
      OptionT(self.get.map(lens.getOption)).getOrElseF(init.use(x =>
        self.modify(a =>
          lens.getOption(a) match {
            case Some(r) => (a, r)
            case None    => (lens.set(x)(a), f(x))
        })))
  }
}
