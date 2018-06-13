package manatki.syntax

import cats.ApplicativeError
import cats.data.EitherT

object eithert {
  implicit class EitherTOps[F[_], E, A](val x: EitherT[F, E, A]) extends AnyVal {
    /** lifting errors from underlying functor */
    def liftError[U](f: U => E)(implicit err: ApplicativeError[F, U]): EitherT[F, E, A] =
      EitherT(err.handleError(x.value)(e => Left(f(e))))
  }
}
