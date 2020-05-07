package manatki.control

import cats.data.{EitherK, Tuple2K}
import cats.{Functor, Id}
import manatki.data.tagless.Rep
import simulacrum.typeclass
import tofu.syntax.monadic._


@typeclass
trait Unwind[F[_]] extends Functor [F]{
  def unwind[R, A](f: R => F[A]): F[R => A]

  def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]] =
    map(unwind(f))(ga.map)

  def tabulate[A](fa: Rep[F] => A): F[A] = {

    def u[X] = unwind[F[X], X](identity)
    def x : F[Rep[F]] = ???

    map(x)(fa)
  }


}

object Unwind {
  implicit val idUnwind: Unwind[Id] = new Unwind[Id] {

    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

    def unwind[R, A](f: R => A): R => A = f
  }

  implicit def constUnwind[X]: Unwind[λ[x => X]] = new Unwind[λ[x => X]] {

    def map[A, B](fa: X)(f: A => B): X = fa

    def unwind[R, A](f: R => X): X = ???
  }

  implicit def prodUnwind[F[_]: Unwind, G[_]: Unwind]: Unwind[Tuple2K[F, G, *]] = new Unwind[Tuple2K[F, G, *]] {

    def map[A, B](fa: Tuple2K[F, G, A])(f: A => B): Tuple2K[F, G, B] = fa.map(f)

    def unwind[R, A](f: R => Tuple2K[F, G, A]): Tuple2K[F, G, R => A] = Tuple2K(
      Unwind[F].unwind(r => f(r).first),
      Unwind[G].unwind(r => f(r).second)
    )
  }

  implicit def sumUnwind[F[_]: Unwind, G[_]: Unwind]: Unwind[EitherK[F, G, *]]  = new Unwind[EitherK[F, G, *]] {

    def map[A, B](fa: EitherK[F, G, A])(f: A => B): EitherK[F, G, B] = fa.map(f)

    def unwind[R, A](f: R => EitherK[F, G, A]): EitherK[F, G, R => A] = ???
  }
}
