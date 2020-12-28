package manatki.data.tagless
package data
import cats.Applicative
import manatki.data.tagless.ProTraverse.Tab
import tofu.syntax.monadic._

trait Cons[-A, -I, +O] {
  def cons(a: A, y: I): O
}

object Cons {
  def apply[A, P[-i, +o] <: Cons[A, i, o]](a: A, tail: Layer[P]): Layer[P] = Layer[P](_.cons(a, tail))

  trait TabTraverse[I, F[_], A, B, C, P[-x, +y] <: Cons[I, x, y]] extends Tab[F, A, B, C, P] with Cons[I, A, C] {
    def cons(head: I, tail: A): C = right(left(tail).map(b => rep(_.cons(head, b))))
  }

  implicit def corepresentable[I]: ProTraverse[Cons[I, *, *]] = new ProTraverse[Cons[I, *, *]] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[Rep[Cons[I, B, *]]] => C): Cons[I, A, C] =
      new Tab(left, right) with TabTraverse[I, F, A, B, C, Cons[I, -*, +*]]
  }
}
