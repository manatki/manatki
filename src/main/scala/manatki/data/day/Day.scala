package manatki.data.day

import cats._
import cats.syntax.apply._
import cats.syntax.coflatMap._
import cats.syntax.comonad._
import cats.syntax.functor._
import cats.syntax.semigroupal._

trait Day[F[_], G[_], A] {
  type X
  type Y
  def fx: F[X]
  def gy: G[Y]
  def comb: (X, Y) => Eval[A]
  def mapKFirst[H[_]](fk: F ~> H): Day[H, G, A]
  def mapKSecond[H[_]](fk: G ~> H): Day[F, H, A]
  def map[B](f: A => B): Day[F, G, B]                 = Day[F, G, X, Y, B](fx, gy)((x, y) => comb(x, y).map(f))
  def fold[H[_]: Apply](fk: F ~> H, gk: G ~> H): H[A] = fk(fx).map2(gk(gy))(comb).map(_.value)
  def swap: Day[G, F, A]                              = Day(gy, fx)((y, x) => comb(x, y))

  def projectL[B](implicit F: Functor[F], G: Comonad[G]): F[A] = fx.map(comb(_, gy.extract).value)
  def projectR[B](implicit F: Comonad[F], G: Functor[G]): G[A] = gy.map(comb(fx.extract, _).value)
}

object Day extends DayInstances1 {
  def apply[F[_], G[_], X, Y, A](fx: F[X], gy: G[Y])(comb: (X, Y) => Eval[A]): Day[F, G, A] = new Impl(fx, gy, comb)
  def combine[F[_], G[_], X, Y, A](fx: F[X], gy: G[Y])(comb: (X, Y) => A): Day[F, G, A] =
    new Impl(fx, gy, (x: X, y: Y) => Eval.later(comb(x, y)))

  private class Impl[F[_], G[_], XX, YY, A](val fx: F[XX], val gy: G[YY], val comb: (XX, YY) => Eval[A]) extends Day[F, G, A] {
    type X = XX
    type Y = YY
    override def mapKFirst[H[_]](fk: F ~> H): Day[H, G, A]  = Day(fk(fx), gy)(comb)
    override def mapKSecond[H[_]](fk: G ~> H): Day[F, H, A] = Day(fx, fk(gy))(comb)
  }

  class DayFunctor[F[_], G[_]] extends Functor[Day[F, G, ?]] {
    override def map[A, B](fa: Day[F, G, A])(f: A => B): Day[F, G, B] = fa.map(f)
  }

  class DayApply[F[_]: Semigroupal, G[_]: Semigroupal] extends DayFunctor[F, G] with Apply[Day[F, G, ?]] {
    override def ap[A, B](ff: Day[F, G, A => B])(fa: Day[F, G, A]): Day[F, G, B] = map2(ff, fa)(_(_))
    override def map2[A, B, Z](fa: Day[F, G, A], fb: Day[F, G, B])(f: (A, B) => Z): Day[F, G, Z] =
      Day[F, G, (fa.X, fb.X), (fa.Y, fb.Y), Z](fa.fx.product(fb.fx), fa.gy.product(fb.gy)) {
        case ((ax, bx), (ay, by)) => fa.comb(ax, ay).map2(fb.comb(bx, by))(f)
      }
  }

  class DayApplicative[F[_]: InvariantMonoidal, G[_]: InvariantMonoidal] extends DayApply[F, G] with Applicative[Day[F, G, ?]] {
    override val unit: Day[F, G, Unit] =
      Day[F, G, Unit, Unit, Unit](InvariantMonoidal[F].unit, InvariantMonoidal[G].unit)((_, _) => Eval.now(()))
    override def pure[A](x: A): Day[F, G, A] = map(unit)(_ => x)
  }

  class DayCoflatMap[F[_]: CoflatMap, G[_]: CoflatMap] extends DayFunctor[F, G] with CoflatMap[Day[F, G, ?]] {
    override def coflatMap[A, B](fa: Day[F, G, A])(f: Day[F, G, A] => B): Day[F, G, B] =
      Day[F, G, F[fa.X], G[fa.Y], B](fa.fx.coflatten, fa.gy.coflatten)((fx, gy) => Eval.later(f(Day(fx, gy)(fa.comb))))
  }

  class DayComonad[F[_]: Comonad, G[_]: Comonad] extends DayCoflatMap[F, G] with Comonad[Day[F, G, ?]] {
    override def extract[A](x: Day[F, G, A]): A = x.comb(x.fx.extract, x.gy.extract).value
  }
}

sealed trait DayInstances1 extends DayInstances2 { self: Day.type =>
  implicit def applicative[F[_]: InvariantMonoidal, G[_]: InvariantMonoidal, A]: Applicative[Day[F, G, ?]] = new DayApplicative
}

sealed trait DayInstances2 extends DayInstances3 { self: Day.type =>
  implicit def applyInstance[F[_]: Semigroupal, G[_]: Semigroupal, A]: Apply[Day[F, G, ?]] = new DayApply
}

sealed trait DayInstances3 extends DayInstances4 { self: Day.type =>
  implicit def coflatMap[F[_]: CoflatMap, G[_]: CoflatMap, A]: CoflatMap[Day[F, G, ?]] = new DayCoflatMap
}

sealed trait DayInstances4 extends DayInstances5 { self: Day.type =>
  implicit def comonad[F[_]: Comonad, G[_]: Comonad, A]: Comonad[Day[F, G, ?]] = new DayComonad
}

sealed trait DayInstances5 { self: Day.type =>
  implicit def functor[F[_], G[_], A]: Functor[Day[F, G, ?]] = new DayFunctor
}
