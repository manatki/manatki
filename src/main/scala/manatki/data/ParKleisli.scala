package manatki.data
import cats.Monad
import cats.arrow.Arrow
import cats.evidence.Is
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.parallel._
import tofu.syntax.splitting._
import tofu.parallel.Paralleled
import manatki.data.ParKleisli.{Single, Split}

sealed trait ParKleisli[F[_], A, B] {
  def forceRun(a: A)(implicit FP: Paralleled[F]): F[B]

  protected[data] def andThen[X](next: ParKleisli[F, B, X])(implicit F: Monad[F]): ParKleisli[F, A, X]

  protected def composeSingle[X](prev: Single[F, X, A])(implicit F: Monad[F]): ParKleisli[F, X, B]

  protected def composeSplit[X1, Y1, X2, Y2](split: Split[F, X1, Y1, X2, Y2], yisa: (Y1, Y2) Is A)(
      implicit F: Monad[F]): ParKleisli[F, (X1, X2), B]
}

object ParKleisli {
  final case class Single[F[_], A, B](fab: A => F[B]) extends ParKleisli[F, A, B] {
    def forceRun(a: A)(implicit FP: Paralleled[F]): F[B] = fab(a)

    protected[data] def andThen[X](next: ParKleisli[F, B, X])(implicit F: Monad[F]): ParKleisli[F, A, X] =
      next.composeSingle(this)

    protected def composeSingle[X](prev: Single[F, X, A])(implicit F: Monad[F]): ParKleisli[F, X, B] =
      Single(a => prev.fab(a).flatMap(fab))

    protected def composeSplit[X1, Y1, X2, Y2](split: Split[F, X1, Y1, X2, Y2], yisa: Is[(Y1, Y2), A])(
        implicit F: Monad[F]): ParKleisli[F, (X1, X2), B] = AndThen(yisa.substitute[ParKleisli[F, (X1, X2), ?]](split), this)

  }
  final case class Split[F[_], A, B, C, D](first: ParKleisli[F, A, B], second: ParKleisli[F, C, D])
      extends ParKleisli[F, (A, C), (B, D)] {
    def forceRun(a: (A, C))(implicit FP: Paralleled[F]): F[(B, D)] =
      first.forceRun(a._1).parProduct(second.forceRun(a._2))

    protected[data] def andThen[X](next: ParKleisli[F, (B, D), X])(implicit F: Monad[F]) =
      next.composeSplit(this, Is.refl)

    protected def composeSingle[X](prev: Single[F, X, (A, C)])(implicit F: Monad[F]): ParKleisli[F, X, (B, D)] =
      AndThen(prev, this)

    protected def composeSplit[X1, Y1, X2, Y2](split: Split[F, X1, Y1, X2, Y2], yisa: Is[(Y1, Y2), (A, C)])(
        implicit F: Monad[F]): ParKleisli[F, (X1, X2), (B, D)] =
      Split(split.first.asInstanceOf[ParKleisli[F, X1, A]] andThen first,
            split.second.asInstanceOf[ParKleisli[F, X2, C]] andThen second)
  }

  final case class AndThen[F[_], A, B, C](start: ParKleisli[F, A, B], end: ParKleisli[F, B, C]) extends ParKleisli[F, A, C] {
    def forceRun(a: A)(implicit FP: Paralleled[F]): F[C] =
      FP.monad.flatMap(start.forceRun(a))(end.forceRun)

    protected[data] def andThen[X](next: ParKleisli[F, C, X])(implicit F: Monad[F]) =
      AndThen(start, end andThen next)

    protected def composeSingle[X](prev: Single[F, X, A])(implicit F: Monad[F]): ParKleisli[F, X, C] =
      AndThen(start.composeSingle(prev), end)

    protected def composeSplit[X1, Y1, X2, Y2](split: Split[F, X1, Y1, X2, Y2], yisa: Is[(Y1, Y2), A])(
        implicit F: Monad[F]): ParKleisli[F, (X1, X2), C] =
      AndThen(yisa.substitute[ParKleisli[F, (X1, X2), ?]](split) andThen start, end)

  }

  implicit def parKleisliArrow[F[_]](implicit FP: Paralleled[F]): Arrow[ParKleisli[F, ?, ?]] =
    new Arrow[ParKleisli[F, ?, ?]] {
      implicit val F: Monad[F]                       = FP.monad
      def lift[A, B](f: A => B): ParKleisli[F, A, B] = Single(a => f(a).pure[F])

      def first[A, B, C](fa: ParKleisli[F, A, B]): ParKleisli[F, (A, C), (B, C)] = Split(fa, id)

      override def second[A, B, C](fa: ParKleisli[F, A, B]): ParKleisli[F, (C, A), (C, B)] = Split(id, fa)

      override def split[A, B, C, D](f: ParKleisli[F, A, B], g: ParKleisli[F, C, D]): ParKleisli[F, (A, C), (B, D)] =
        Split(f, g)

      def compose[A, B, C](f: ParKleisli[F, B, C], g: ParKleisli[F, A, B]): ParKleisli[F, A, C] = g.andThen(f)
    }
}
