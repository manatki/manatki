package manatki.data.tagless
package data
import manatki.data.tagless.ProCorepresentable.{LMap, Tab}

trait Nil[+O] {
  def nil: O
}

object Nil extends Layer[λ[(`-a`, `+b`) => Nil[b]]] {
  def unpack[A](p: Nil[A]): A = p.nil

  type NP[-a, +b] = Nil[b]

  trait Tabulate[A, B, P[x, y] <: Nil[y]] extends Tab[A, B, P] with Nil[B] {
    def nil: B = k(Rep.mk(_.nil))
  }

  trait LeftMap[A, B, C, P[x, y] <: Nil[y]] extends LMap[A, B, C, P] with Nil[B] {
    def nil: B = pab.nil
  }

  implicit val corepresentable: ProCorepresentable[NP] = new ProCorepresentable[NP] {
    def tabulate[A, B](k: Rep[Nil] => B): Nil[B] =
      new Tab[A, B, NP](k) with Tabulate[A, B, NP]

    def leftMap[A, B, C](fab: Nil[B])(f: C => A): Nil[B] = fab
  }

}
