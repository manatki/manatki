package manatki.data.tagless
package data
import manatki.data.tagless.ProCorepresentable.{LMap, Tab}

trait Single[-A, +O] {
  def single(a: A): O
}

object Single {
  type NP[-a, -i, +o] = Single[a, o]
  def apply[A](a: A): Layer[NP[A, -*, +*]] = Layer[NP[A, -*, +*]](_.single(a))

  implicit def corepresentable[I]: ProCorepresentable[NP[I, *, *]] = new ProCorepresentable[NP[I, *, *]] {
    def tabulate[A, B](k: Rep[Single[I, *]] => B): Single[I, B] =
      new Tab[A, B, NP[I, *, *]](k) with Tabulate[I, A, B, NP[I, *, *]]

    def leftMap[A, B, C](fab: Single[I, B])(f: C => A): Single[I, B] = fab
  }

  trait Tabulate[I, A, B, P[x, y] <: Single[I, y]] extends Tab[A, B, P] with Single[I, B] {
    def single(a: I): B = k(Rep.mk(_.single(a)))
  }

  trait LeftMap[I, A, B, C, P[x, y] <: Single[I, y]] extends LMap[A, B, C, P] with Single[I, B] {
    def single(a: I): B = pab.single(a)
  }
}
