package manatki.data.tagless
package data
import cats.Applicative
import manatki.data.tagless.ProTraverse.Tab
import tofu.syntax.monadic._

trait Nil[+O] {
  def nil: O
}

object Nil extends Layer[λ[(`-a`, `+b`) => Nil[b]]] {
  def unpack[A](p: Nil[A]): A = p.nil

  type NP[-a, +b] = Nil[b]

  trait TabTraverse[F[_], A, B, C, P[-x, +y] <: Nil[y]] extends Tab[F, A, B, C, P] with Nil[C] {
    def nil: C = right(F.pure(rep(_.nil)))
  }

  implicit val corepresentable: ProTraverse[NP] = new ProTraverse[NP] {
    def tabTraverse[F[_] : Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): NP[A, C] =
      new Tab(left, right) with TabTraverse[F, A, B, C, NP]
  }

}
