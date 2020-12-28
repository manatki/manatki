package manatki.data.tagless.data
import cats.Applicative
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless.{Builder, ProTraverse}

trait NelP[-A, -I, +O] extends Single[A, O] with Cons[A, I, O]

object NelP {
  def apply[A](a: A, as: A*): XNel[A] =
    Builder[NelP[A, -*, +*], Seq[A]] {
      case (Seq(a), p)          => p.single(a)
      case (Seq(a, as @ _*), p) => p.cons(a, as)
    }.unfold(a +: as)

  implicit def corepresentable[I]: ProTraverse[NelP[I, -*, +*]] = new ProTraverse[NelP[I, -*, +*]] {
    def tabTraverse[F[_] : Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): NelP[I, A, C] =
      new Tab[F, A, B, C, NelP[I, -*, +*]](left, right) with TabTraverse[I, F, A, B, C, NelP[I, -*, +*]]
  }

  trait TabTraverse[I, F[_], A, B, C, P[-x, +y] <: NelP[I, x, y]]
      extends Tab[F, A, B, C, P] with NelP[I, A, C] with Single.TabTraverse[I, F, A, B, C, P]
      with Cons.TabTraverse[I, F, A, B, C, P]
}
