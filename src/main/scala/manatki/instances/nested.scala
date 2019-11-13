package manatki.instances
import cats.{Monad, StackSafeMonad}
import cats.data.Nested
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import manatki.control.Distributed

object nested {
  implicit def nestedMonad[F[_]: Monad, G[_]: Monad](implicit dist: Distributed[G, F]): Monad[Nested[F, G, *]] =
    new StackSafeMonad[Nested[F, G, *]] {
      def flatMap[A, B](fa: Nested[F, G, A])(f: A => Nested[F, G, B]): Nested[F, G, B] =
        Nested(fa.value.flatMap(ga => dist.exchange(ga.map(a => f(a).value))).map(_.flatten))
      def pure[A](x: A): Nested[F, G, A] = Nested(x.pure[G].pure[F])
    }
}
