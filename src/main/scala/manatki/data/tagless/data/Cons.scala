package manatki.data.tagless
package data
import cats.Applicative
import manatki.data.tagless.ProCorep.{LMap, Tab}
import manatki.data.tagless.ProTraverse.ProTrav
import tofu.syntax.monadic._

trait Cons[-A, -I, +O] {
  def cons(a: A, y: I): O
}

object Cons {
  def apply[A, P[-i, +o] <: Cons[A, i, o]](a: A, tail: Layer[P]): Layer[P] = Layer[P](_.cons(a, tail))

  trait Tabulate[I, A, B, P[-x, +y] <: Cons[I, x, y]] extends Tab[A, B, P] with Cons[I, A, B] {
    def cons(head: I, tail: A): B = k(Rep.pro[P, A](_.cons(head, tail)))
  }

  trait LeftMap[I, A, B, C, P[-x, +y] <: Cons[I, x, y]] extends LMap[A, B, C, P] with Cons[I, C, B] {
    def cons(a: I, y: C): B = pab.cons(a, f(y))
  }

  trait Trav[F[_], I, A, B, P[-x, +y] <: Cons[I, x, y]] extends ProTrav[F, A, B, P] with Cons[I, F[A], F[B]] {
    def cons(i: I, fa: F[A]): F[B] = fa.map(pab.cons(i, _))
  }

  implicit def corepresentable[I]: ProTraverse[Cons[I, -*, +*]] = new ProTraverse[Cons[I, -*, +*]] {
    def cotabulate[A, B](k: Rep[Cons[I, A, *]] => B): Cons[I, A, B] =
      new Tab[A, B, Cons[I, -*, +*]](k) with Tabulate[I, A, B, Cons[I, -*, +*]]

    def lmap[A, B, C](fab: Cons[I, A, B])(f: C => A): Cons[I, C, B] =
      new LMap[A, B, C, Cons[I, -*, +*]](fab, f) with LeftMap[I, A, B, C, Cons[I, -*, +*]]

    def protraverse[F[_]: Applicative, A, B](p: Cons[I, A, B]): Cons[I, F[A], F[B]] =
      new ProTrav[F, A, B, Cons[I, -*, +*]](p) with Trav[F, I, A, B, Cons[I, -*, +*]]
  }
}
