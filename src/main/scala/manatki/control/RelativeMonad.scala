package manatki.control

import cats.mtl.ApplicativeLocal
import cats.{Applicative, Monad, MonadError, StackSafeMonad}

trait RelativeMonad[J[_], F[_]] {
  def rpure[A](ja: J[A]): F[A]
  def rflatMap[A, B](fa: F[A])(f: J[A] => F[B]): F[B]
}

object RelativeMonad {
  trait Module[J[_], TC[f[_]]] {
    def from[F[_]](rm: RelativeMonad[J, F]): TC[F]

    def to[F[_]](tc: TC[F]): RelativeMonad[J, F]
  }

  class Error[E] extends Module[Either[E, *], MonadError[*[_], E]] {
    def from[F[_]](rm: RelativeMonad[Either[E, *], F]) =
      new MonadError[F, E] with StackSafeMonad[F] {
        def pure[A](x: A): F[A]                         = rm.rpure(Right(x))
        def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
          rm.rflatMap(fa) {
            case Left(e)  => raiseError(e)
            case Right(a) => f(a)
          }
        def raiseError[A](e: E): F[A]                   = rm.rpure(Left(e))

        def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A] =
          rm.rflatMap(fa) {
            case Left(e)  => f(e)
            case Right(a) => pure(a)
          }
      }

    def to[F[_]](tc: MonadError[F, E]) =
      new RelativeMonad[Either[E, *], F] {
        def rpure[A](ja: Either[E, A]): F[A] =
          ja match {
            case Left(e)  => tc.raiseError(e)
            case Right(a) => tc.pure(a)
          }

        def rflatMap[A, B](fa: F[A])(f: Either[E, A] => F[B]): F[B] =
          tc.flatMap(fa)(a => f(Right(a)))
      }
  }

  trait MonadReader[F[_], R] extends ApplicativeLocal[F, R] with StackSafeMonad[F]
  class Reader[R] extends Module[R => *, MonadReader[*[_], R]] {
    def from[F[_]](rm: RelativeMonad[Function[R, *], F]): MonadReader[F, R] =
      new MonadReader[F, R] {
        def local[A](f: R => R)(fa: F[A]): F[A]         =
          rm.rflatMap(fa)(g => rm.rpure(r => g(f(r))))
        def scope[A](e: R)(fa: F[A]): F[A]              = local(_ => e)(fa)
        val applicative: Applicative[F]                 = this
        def ask: F[R]                                   = rm.rpure(identity)
        def reader[A](f: R => A): F[A]                  = rm.rpure(f)
        def pure[A](x: A): F[A]                         = rm.rpure(_ => x)
        def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
          rm.rflatMap(fa) { g =>
            rm.rflatMap(ask)(h => ???)
          }
      }

    def to[F[_]](tc: MonadReader[F, R]): RelativeMonad[Function[R, *], F] = ???
  }
}
