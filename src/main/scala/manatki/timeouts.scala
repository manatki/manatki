package manatki
import cats.effect.{Concurrent, Timer}
import Concurrent.ops._
import cats.Eval
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.duration.FiniteDuration

object timeouts {
  implicit class TimeoutSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def timeoutOr[B](after: FiniteDuration)(x: => F[B])(implicit conc: Concurrent[F], timer: Timer[F]): F[Either[B, A]] =
      timer.sleep(after).productREval(Eval.later(x)).race(fa)

    def timeout(after: FiniteDuration)(implicit conc: Concurrent[F], timer: Timer[F]): F[Option[A]] =
      timeoutOr(after)(conc.unit).map(_.toOption)

    def timeoutTo(after: FiniteDuration)(x: => F[A])(implicit conc: Concurrent[F], timer: Timer[F]): F[A] =
      timeoutOr(after)(x).map(_.merge)
  }
}
