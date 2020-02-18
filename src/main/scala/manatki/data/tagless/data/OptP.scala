package manatki.data.tagless
package data
import cats.{Applicative, Show}
import cats.syntax.show._
import manatki.data.tagless.ProCorep.{LMap, Tab}
import manatki.data.tagless.ProTraverse.ProTrav

trait OptP[-A, +O] extends Single[A, O] with Nil[O]

object OptP {
  type OP[-A, -I, +O] = OptP[A, O]

  implicit def corepresentable[I]: ProTraverse[OP[I, -*, +*]] = new ProTraverse[OP[I, -*, +*]] {
    def cotabulate[A, B](k: Rep[OptP[I, *]] => B): OptP[I, B] =
      new Tab[A, B, OP[I, -*, +*]](k) with Tabulate[I, A, B, OP[I, -*, +*]]

    def lmap[A, B, C](fab: OptP[I, B])(f: C => A): OptP[I, B] = fab

    def protraverse[F[_]: Applicative, A, B](p: OptP[I, B]): OP[I, F[A], F[B]] =
      new ProTrav[F, A, B, OP[I, -*, +*]](p) with Trav[F, I, A, B, OP[I, -*, +*]]
  }

  trait Tabulate[I, A, B, P[-x, +y] <: OP[I, x, y]]
      extends Tab[A, B, P] with OptP[I, B] with Single.Tabulate[I, A, B, P] with Nil.Tabulate[A, B, P]

  trait LeftMap[I, A, B, C, P[-x, +y] <: OP[I, x, y]]
      extends LMap[A, B, C, P] with OptP[I, B] with Single.LeftMap[I, A, B, C, P] with Nil.LeftMap[A, B, C, P]

  trait Trav[F[_], I, A, B, P[-x, +y] <: OptP[I, y]]
      extends ProTrav[F, A, B, P] with OptP[I, F[B]] with Single.Trav[F, I, A, B, P] with Nil.Trav[F, A, B, P]

  implicit def show[A: Show]: Show[XOpt[A]] =
    _.unpack(new OptP[A, String] {
      def nil: String          = "None"
      def single(a: A): String = show"Some($a)"
    })
}
