package manatki.data.cont

import cats.data.ContT
import cats.mtl.{ApplicativeLocal, MonadState}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.{Defer, Monad, MonadError, StackSafeMonad}

object contState {
  implicit def conttStateError[F[_]: Monad, R, X](
      implicit loc: ApplicativeLocal[F, R],
      FD: Defer[F]): MonadState[ContT[F, X, ?], R] with MonadError[ContT[F, X, ?], X] =
    new MonadState[ContT[F, X, ?], R] with MonadError[ContT[F, X, ?], X] {
//      implicit val contMonad: Monad[ContT[F, X, ?]] = ContT.catsDataContTMonad[F, X]
      import FD.defer

      val monad: Monad[ContT[F, X, ?]] = this

      def get: ContT[F, X, R] = ContT(k => defer(loc.ask.flatMap(k)))

      def set(s: R): ContT[F, X, Unit] = ContT(k => defer(loc.scope(s)(k(()))))

      def inspect[A](f: R => A): ContT[F, X, A] = ContT(k => defer(loc.ask.flatMap(r => k(f(r)))))

      def modify(f: R => R): ContT[F, X, Unit] = ContT(k => defer(loc.local(f)(k(()))))

      def raiseError[A](e: X): ContT[F, X, A] = ContT(_ => e.pure[F])

      def handleErrorWith[A](fa: ContT[F, X, A])(f: X => ContT[F, X, A]): ContT[F, X, A] =
        ContT(k => defer(fa.run(a => k(a).flatMap(f(_).run(k)))))

      def pure[A](x: A): ContT[F, X, A] = ContT.pure(x)

      def flatMap[A, B](fa: ContT[F, X, A])(f: A => ContT[F, X, B]): ContT[F, X, B] = fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => ContT[F, X, Either[A, B]]): ContT[F, X, B] = ContT.tailRecM(a)(f)
    }

  implicit def contXTStateError[F[_]: Monad, R, X](
      implicit loc: ApplicativeLocal[F, R]): MonadState[ContXT[F, X, ?], R] with MonadError[ContXT[F, X, ?], X] =
    new MonadState[ContXT[F, X, ?], R] with MonadError[ContXT[F, X, ?], X] with StackSafeMonad[ContXT[F, X, ?]] {
      //      implicit val contMonad: Monad[ContT[F, X, ?]] = ContT.catsDataContTMonad[F, X]

      val monad: Monad[ContXT[F, X, ?]] = this

      def get: ContXT[F, X, R] = k => loc.ask.flatMap(k)

      def set(s: R): ContXT[F, X, Unit] = k => loc.scope(s)(k(()))

      def inspect[A](f: R => A): ContXT[F, X, A] = k => loc.ask.flatMap(r => k(f(r)))

      def modify(f: R => R): ContXT[F, X, Unit] = k => loc.local(f)(k(()))

      def raiseError[A](e: X): ContXT[F, X, A] = _ => e.pure[F]

      def handleErrorWith[A](fa: ContXT[F, X, A])(f: X => ContXT[F, X, A]): ContXT[F, X, A] =
        k => fa.run(a => k(a).flatMap(f(_).run(k)))

      def pure[A](x: A): ContXT[F, X, A] = _(x)

      def flatMap[A, B](fa: ContXT[F, X, A])(f: A => ContXT[F, X, B]): ContXT[F, X, B] = fa.flatMap(f)
    }

}
