package manatki.syntax

import cats.FlatMap
import manatki.higherKinded.Embed

object embed {
  implicit class EmbedOps[T[_[_]], F[_]](private val ft: F[T[F]]) extends AnyVal {
    def embed(implicit emb: Embed[T], F: FlatMap[F]): T[F] = emb.embed(ft)
  }
}
