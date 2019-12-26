package manatki.data.tagless
package data
import manatki.data.tagless.ProCorepresentable.{LMap, Tab}

trait Cons[-A, -I, +O] {
  def cons(a: A, y: I): O
}

object Cons {
  def apply[A, P[-i, +o] <: Cons[A, i, o]](a: A, tail: Layer[P]): Layer[P] = Layer[P](_.cons(a, tail))

  trait Tabulate[I, A, B, P[x, y] <: Cons[I, x, y]] extends Tab[A, B, P] with Cons[I, A, B] {
    def cons(head: I, tail: A): B = k(Rep.mk(_.cons(head, tail)))
  }

  trait LeftMap[I, A, B, C, P[x, y] <: Cons[I, x, y]] extends LMap[A, B, C, P] with Cons[I, C, B] {
    def cons(a: I, y: C): B = pab.cons(a, f(y))
  }

  implicit def corepresentable[I]: ProCorepresentable[Cons[I, -*, +*]] = new ProCorepresentable[Cons[I, -*, +*]] {
    def tabulate[A, B](k: Rep[Cons[I, A, *]] => B): Cons[I, A, B] =
      new Tab[A, B, Cons[I, *, *]](k) with Tabulate[I, A, B, Cons[I, *, *]]

    def leftMap[A, B, C](fab: Cons[I, A, B])(f: C => A): Cons[I, C, B] =
      new LMap[A, B, C, Cons[I, *, *]](fab, f) with LeftMap[I, A, B, C, Cons[I, *, *]]
  }
}