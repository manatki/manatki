package manatki.control

import cats.data.{EitherK, Tuple2K}
import cats.{Contravariant, Distributive, Functor, Id}
import manatki.data.tagless.Rep
import simulacrum.typeclass
import tofu.syntax.monadic._
trait Unwind[F[_]] {
  def unwind[R, A](f: R => F[A]): F[R => A]

  def distribute(implicit F: Functor[F]): Distributive[F] = new Distributive[F] {
    def distribute[G[_], A, B](ga: G[A])(f: A => F[B])(implicit evidence$1: Functor[G]): F[G[B]] =
      map(unwind(f))(ga.map)

    def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
  }

//  def tabulate[A](fa: Rep[F] => A): F[A] = {
//
//    def u[X]         = unwind[F[X], X](identity)
//    def x: F[Rep[F]] = ???
//
////    map(x)(fa)
//  }
}

object Unwind {
  def apply[F[_]](implicit F: Unwind[F]): Unwind[F] = F
  implicit val idUnwind: Unwind[Id]                 = new Unwind[Id] {
    def unwind[R, A](f: R => A): R => A = f
  }

  private def constUnwind[X]: Unwind[λ[x => X]] = new Unwind[λ[x => X]] {
    def unwind[R, A](f: R => X): X = ???
  }

  implicit def prodUnwind[F[_]: Unwind, G[_]: Unwind]: Unwind[Tuple2K[F, G, *]] = new Unwind[Tuple2K[F, G, *]] {
    def unwind[R, A](f: R => Tuple2K[F, G, A]): Tuple2K[F, G, R => A] = Tuple2K(
      Unwind[F].unwind(r => f(r).first),
      Unwind[G].unwind(r => f(r).second)
    )
  }

  private def sumUnwind[F[_]: Unwind, G[_]: Unwind]: Unwind[EitherK[F, G, *]] = new Unwind[EitherK[F, G, *]] {
    def unwind[R, A](f: R => EitherK[F, G, A]): EitherK[F, G, R => A] = ???
  }
}
