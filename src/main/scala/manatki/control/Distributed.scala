package manatki.control
import cats.data.Nested
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.nonEmptyTraverse._
import cats.syntax.traverse._
import cats.{Applicative, Apply, Functor, NonEmptyTraverse, Traverse}

import scala.util.Try

trait Distributed[F[_], G[_]] {
  def exchange[A](fga: F[G[A]]): G[F[A]]
  def swap[A](nested: Nested[F, G, A]): Nested[G, F, A] = Nested(exchange(nested.value))
}

object Distributed extends Distributed1 {
  implicit def distributedTuple[W, F[_]: Functor]: Distributed[(W, ?), F] = new Distributed[(W, ?), F] {
    def exchange[A](fga: (W, F[A])): F[(W, A)] = fga._2.map((fga._1, _))
  }
  implicit def distributedFunction[R, F[_]: Functor]: Distributed[F, R => ?] = new Distributed[F, R => ?] {
    def exchange[A](gfa: F[R => A]): R => F[A] = r => gfa.map(_(r))
  }
  implicit def distributedEither[E, F[_]: Applicative]: Distributed[Either[E, ?], F] = new Distributed[Either[E, ?], F] {
    def exchange[A](fga: Either[E, F[A]]): F[Either[E, A]] = fga.fold(_.asLeft.pure, _.map(_.asRight))
  }
  implicit def distributedTry[F[_]: Applicative]: Distributed[Try, F] = new Distributed[Try, F] {
    def exchange[A](fga: Try[F[A]]): F[Try[A]] = fga.fold(e => (util.Failure(e): Try[A]).pure[F], _.map(util.Success(_)))
  }

}

trait Distributed1 extends Distributed2 {
  implicit def distributedTraverse[F[_]: Traverse, G[_]: Applicative]: Distributed[F, G] = new Distributed[F, G] {
    def exchange[A](fga: F[G[A]]): G[F[A]] = fga.sequence
  }
}

trait Distributed2 {
  implicit def distributedNonEmptyTraverse[F[_]: NonEmptyTraverse, G[_]: Apply]: Distributed[F, G] = new Distributed[F, G] {
    def exchange[A](fga: F[G[A]]): G[F[A]] = fga.nonEmptySequence
  }
}
