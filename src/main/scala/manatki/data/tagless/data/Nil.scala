package manatki.data.tagless
package data
import cats.Applicative
import manatki.data.tagless.ProCorepresentable.{LMap, Tab}
import manatki.data.tagless.ProTraverse.ProTrav
import tofu.syntax.monadic._

trait Nil[+O] {
  def nil: O
}

object Nil extends Layer[λ[(`-a`, `+b`) => Nil[b]]] {
  def unpack[A](p: Nil[A]): A = p.nil

  type NP[-a, +b] = Nil[b]

  trait Tabulate[A, B, P[x, y] <: Nil[y]] extends Tab[A, B, P] with Nil[B] {
    def nil: B = k(Rep.pro[P, A](_.nil))
  }

  trait LeftMap[A, B, C, P[x, y] <: Nil[y]] extends LMap[A, B, C, P] with Nil[B] {
    def nil: B = pab.nil
  }

  trait Trav[F[_], A, B, P[x, y] <: Nil[y]] extends ProTrav[F, A, B, P] with Nil[F[B]] {
    def nil: F[B] = pab.nil.pure[F]
  }

  implicit val corepresentable: ProTraverse[NP] = new ProTraverse[NP] {
    def tabulate[A, B](k: Rep[Nil] => B): Nil[B] =
      new Tab[A, B, NP](k) with Tabulate[A, B, NP]

    def leftMap[A, B, C](fab: Nil[B])(f: C => A): Nil[B] = fab

    def protraverse[F[_]: Applicative, A, B](p: NP[A, B]): NP[F[A], F[B]] =
      new ProTrav[F, A, B, NP](p) with Trav[F, A, B, NP]
  }

}
