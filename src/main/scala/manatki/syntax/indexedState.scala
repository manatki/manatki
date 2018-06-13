package manatki.syntax

import cats.FlatMap
import cats.data.IndexedStateT

object indexedState {
  implicit class StateTOps[F[_], SA, SB, A](val state: IndexedStateT[F, SA, SB, A]) extends AnyVal {
    def lmapF[T](f: T => F[SA])(implicit F: FlatMap[F]): IndexedStateT[F, T, SB, A] =
      IndexedStateT.applyF[F, T, SB, A](F.map(state.runF)(run => (t: T) => F.flatMap(f(t))(sa => run(sa))))

    def rmapF[T](f: SB => F[T])(implicit F: FlatMap[F]): IndexedStateT[F, SA, T, A] =
      IndexedStateT.applyF[F, SA, T, A](F.map(state.runF)(run => sa => F.flatMap(run(sa)) { case (sb, a) => F.map(f(sb))((_, a)) }))

    def transformSF[R](f: R => F[SA], g: (R, SB) => F[R])(implicit F: FlatMap[F]): IndexedStateT[F, R, R, A] =
      IndexedStateT.applyF[F, R, R, A](F.map(state.runF)(run => r => F.flatMap(f(r))(sa => F.flatMap(run(sa)) { case (sb, a) => F.map(g(r, sb))((_, a)) })))

    def before[T](f: T => F[SA])(implicit F: FlatMap[F]): IndexedStateT[F, T, SB, A] = lmapF(f)

    def after[T](f: SB => F[T])(implicit F: FlatMap[F]): IndexedStateT[F, SA, T, A] = rmapF(f)

    def decorate[R](f: R => F[SA], g: (R, SB) => F[R])(implicit F: FlatMap[F]): IndexedStateT[F, R, R, A] = transformSF(f, g)
  }

}
