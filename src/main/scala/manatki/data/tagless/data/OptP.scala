package manatki.data.tagless
package data
import cats.Show
import cats.syntax.show._
import manatki.data.tagless.ProCorepresentable.{LMap, Tab}

trait OptP[-A, +O] extends Single[A, O] with Nil[O]

object OptP {
  type OP[-A, -I, +O] = OptP[A, O]

  implicit def corepresentable[I]: ProCorepresentable[OP[I, *, *]] = new ProCorepresentable[OP[I, *, *]] {
    def tabulate[A, B](k: Rep[OptP[I, *]] => B): OptP[I, B] =
      new Tab[A, B, OP[I, *, *]](k) with Tabulate[I, A, B, OP[I, *, *]]

    def leftMap[A, B, C](fab: OptP[I, B])(f: C => A): OptP[I, B] = fab
  }

  trait Tabulate[I, A, B, P[x, y] <: OP[I, x, y]]
      extends Tab[A, B, P] with OptP[I, B] with Single.Tabulate[I, A, B, P] with Nil.Tabulate[A, B, P]

  trait LeftMap[I, A, B, C, P[x, y] <: OP[I, x, y]]
      extends LMap[A, B, C, P] with OptP[I, B] with Single.LeftMap[I, A, B, C, P] with Nil.LeftMap[A, B, C, P]

  implicit def show[A: Show]: Show[XOpt[A]] =
    _.unpack(new OptP[A, String] {
      def nil: String          = "None"
      def single(a: A): String = show"Some($a)"
    })
}
