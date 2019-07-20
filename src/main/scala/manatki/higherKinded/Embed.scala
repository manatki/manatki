package manatki
package higherKinded

import cats.{FlatMap, Monad}
import cats.laws._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import simulacrum.typeclass
import syntax.embed._

@typeclass
trait Embed[T[_[_]]] {
  def embed[F[_]: FlatMap](ft: F[T[F]]): T[F]
}

class EmbedLaws[T[_[_]]: Embed] {
  def embedUnit[F[_]: Monad](alg: T[F]): IsEq[T[F]] =
    alg <-> alg.pure[F].embed

  def embedCoherent[F[_]: FlatMap](alg: F[F[T[F]]]): IsEq[T[F]] =
    alg.map(_.embed).embed <-> alg.flatten.embed
}
