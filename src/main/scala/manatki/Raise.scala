package manatki

import cats.ApplicativeError

trait Raise[F[_], E] {
  def raise[A](err: E): F[A]
}

object Raise {
  implicit def raiseApplicativeError[F[_], E, E1](implicit appErr: ApplicativeError[F, E], sub: E1 <:< E): Raise[F, E1] =
    new Raise[F, E1] {
      override def raise[A](err: E1): F[A] = appErr.raiseError(err)
    }
}