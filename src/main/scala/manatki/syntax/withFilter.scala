package manatki.syntax

import cats.{FunctorFilter, MonadError}

object functorFilter {
  implicit class FunctorFilterOps[F[_], A](val fa: F[A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit F: FunctorFilter[F]): F[A] = F.filter(fa)(f)
  }
}

object withFilter {
  implicit class MonadErrorWithFilter[F[_], A](val fa: F[A]) extends AnyVal {
    def withFilter(f: A => Boolean)(implicit F: MonadError[F, Throwable]): F[A] =
      F.ensure(fa)(new NoSuchElementException)(f)
  }
}

