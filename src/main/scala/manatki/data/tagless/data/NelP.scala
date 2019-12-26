package manatki.data.tagless.data
import manatki.data.tagless.ProCorepresentable.{LMap, Tab}
import manatki.data.tagless.{Layer, ProCorepresentable, Rep}

trait NelP[-A, -I, +O] extends Single[A, O] with Cons[A, I, O]

object NelP {
  def apply[A](a: A, as: A*): XNel[A] = as match {
    case Seq()      => Single(a)
    case a1 +: rest => Cons(a, apply(a1, rest: _*))
  }

  implicit def corepresentable[I]: ProCorepresentable[NelP[I, *, *]] = new ProCorepresentable[NelP[I, *, *]] {
    def tabulate[A, B](k: Rep[NelP[I, A, *]] => B): NelP[I, A, B] =
      new Tab[A, B, NelP[I, *, *]](k) with Tabulate[I, A, B, NelP[I, *, *]]

    def leftMap[A, B, C](fab: NelP[I, A, B])(f: C => A): NelP[I, C, B] =
      new LMap[A, B, C, NelP[I, *, *]](fab, f) with LeftMap[I, A, B, C, NelP[I, *, *]]
  }

  trait Tabulate[I, A, B, P[x, y] <: NelP[I, x, y]]
      extends Tab[A, B, P] with NelP[I, A, B] with Single.Tabulate[I, A, B, P] with Cons.Tabulate[I, A, B, P]

  trait LeftMap[I, A, B, C, P[x, y] <: NelP[I, x, y]]
      extends LMap[A, B, C, P] with NelP[I, C, B] with Single.LeftMap[I, A, B, C, P] with Cons.LeftMap[I, A, B, C, P]
}
