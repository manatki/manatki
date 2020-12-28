package manatki.data.tagless
package data
import cats.{Applicative, Show}
import cats.syntax.show._
import manatki.data.tagless.ProTraverse.Tab

trait OptP[-A, +O] extends Single[A, O] with Nil[O]

object OptP {
  type OP[-A, -I, +O] = OptP[A, O]

  implicit def corepresentable[I]: ProTraverse[OP[I, -*, +*]] = new ProTraverse[OP[I, -*, +*]] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): OP[I, A, C] =
      new Tab[F, A, B, C, OP[I, -*, +*]](left, right) with TabTraverse[I, F, A, B, C, OP[I, -*, +*]]
  }

  trait TabTraverse[I, F[_], A, B, C, P[-x, +y] <: OP[I, x, y]]
      extends Tab[F, A, B, C, P] with OptP[I, C] with Single.TabTraverse[I, F, A, B, C, P]
      with Nil.TabTraverse[F, A, B, C, P]

  implicit def show[A: Show]: Show[XOpt[A]] =
    _.unpack(new OptP[A, String] {
      def nil: String          = "None"
      def single(a: A): String = show"Some($a)"
    })
}
