package manatki.data.tagless
package data
import cats.Applicative
import manatki.data.tagless.ProTraverse.Tab

trait Single[-A, +O] {
  def single(a: A): O
}

object Single {
  type SP[-a, -i, +o] = Single[a, o]
  def apply[A](a: A): Layer[SP[A, -*, +*]] = Layer[SP[A, -*, +*]](_.single(a))

  implicit def corepresentable[I]: ProCorep[SP[I, -*, +*]] = new ProTraverse[SP[I, -*, +*]] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): SP[I, A, C] =
      new Tab[F, A, B, C, SP[I, -*, +*]](left, right) with TabTraverse[I, F, A, B, C, SP[I, -*, +*]]
  }

  trait TabTraverse[I, F[_], A, B, C, P[-x, +y] <: Single[I, y]] extends Tab[F, A, B, C, P] with Single[I, C] {
    def single(a: I): C = right(F.pure(rep(_.single(a))))
  }
}
