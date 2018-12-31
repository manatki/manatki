package manatki
package syntax

import cats.Applicative

object raise {
  final implicit class RaiseOps[E](val err: E) extends AnyVal{
    def raise[F[_], A](implicit raise: Raise[F, E]): F[A] = raise.raise(err)
  }

  final implicit class RaiseOptionOps[A](val opt: Option[A]) extends AnyVal {
    def liftTo[F[_]] = new RaiseLiftToApplied[F, A](opt)
  }

  final implicit class RaiseEitherOps[E, A](val either: Either[E, A]) extends AnyVal{
    def toRaise[F[_]](implicit
      app: Applicative[F], raise: Raise[F, E]): F[A] =
      either match {
        case Left(err) => raise.raise(err)
        case Right(value) => app.pure(value)
      }
  }

  class RaiseLiftToApplied[F[_], A](val opt: Option[A]) extends AnyVal {
    def apply[E](err: => E)(implicit raise: Raise[F, E], app: Applicative[F]): F[A] =
      opt match {
        case None    => raise.raise(err)
        case Some(a) => app.pure(a)
      }
  }
}
