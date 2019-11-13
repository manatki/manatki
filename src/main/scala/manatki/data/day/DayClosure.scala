package manatki.data.day
import cats.{Eval, ~>}

trait DayClosure[F[_], G[_], A] {
  def apply[B, C](fb: F[B])(f: (A, B) => Eval[C]): G[C]
}

object DayClosure {
  def curry[F[_], G[_], H[_]](f: Day[F, G, *] ~> H): F ~> DayClosure[G, H, *] =
    new (F ~> DayClosure[G, H, *]) {
      def apply[A](fa: F[A]): DayClosure[G, H, A] = new DayClosure[G, H, A] {
        def apply[B, C](gb: G[B])(g: (A, B) => Eval[C]): H[C] = f(Day(fa, gb)(g))
      }
    }

  def uncurry[F[_], G[_], H[_]](fk: F ~> DayClosure[G, H, *]): Day[F, G, *] ~> H =
    new (Day[F, G, *] ~> H) {
      def apply[A](fa: Day[F, G, A]): H[A] = fk(fa.fx)(fa.gy)(fa.comb)
    }
}
