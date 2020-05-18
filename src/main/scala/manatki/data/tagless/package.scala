package manatki.data
import cats.Id

package object tagless {
  type PTrans[+P[_, _], F[_]] = PTrans.T[P, F]
  type PDistr[P[-_, _], F[_]] = PDistr.T[P, F]

  type Builder[-P[_, _], A] = GBuilder[P, Id, A]

  type UnitF[A]                 = Unit
  type HFunction[F[_], G[_], A] = F[A] => G[A]

  type Platform[-P[-i[_], +o[_]], A] = DKnot[P, Any, A]

  type Act[-L[-i[_], +o[_]], A] = DKnot[Any, L, A]
}
