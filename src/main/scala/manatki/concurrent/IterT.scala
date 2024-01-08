package manatki.concurrent

import tofu.higherKind.derived.representableK
import cats.Functor
import tofu.higherKind.RepresentableK
import cats.Monad
import tofu.concurrent.Atom
import cats.kernel.Monoid
import cats.~>
import tofu.higherKind
import tofu.syntax.monadic._
import cats.syntax.option._
import derevo.derive


trait Pipe[F[_], -ItSrc, +ItRes, -Feed, +Final, +Item] {
  def start[IR >: ItRes]: F[IR]
  def proceed[P >: Pipe.Proceed[ItRes, Final, Item]](it: ItSrc, f: Feed): F[P]
  def next[P >: Pipe.Proceed[ItRes, Final, Item]](it: ItSrc)(implicit ev: Unit <:< Feed): F[P] = proceed(it, ())
  def stop[R >: Final](it: ItSrc): F[R]
}

object Pipe {
  sealed trait Proceed[+State, +Final, +Item]                    extends Product with Serializable
  final case class Next[+State, +Item](state: State, item: Item) extends Proceed[State, Nothing, Item]
  final case class End[+Final](f: Final)                         extends Proceed[Nothing, Final, Nothing]

  // implicit def representableInstance[I, R, A]: RepresentableK[Pipe[*[_], I, R, A]] =
  //   new RepresentableK[Pipe[*[_], I, R, A]] {
  //     def tabulate[F[_]](hom: higherKind.RepK[Pipe[*[_], I, R, A], *] ~> F): Pipe[F, I, R, A] = new Pipe[F, I, R, A] {
  //       type Iterator
  //       def start: F[Iterator] = ???

  //       def proceed(it: Iterator, f: I): F[Proceed[Iterator, R, A]] = ???

  //       def stop(it: Iterator): F[R] = ???
  //     }
  //   }

  // class SingleElem[F[_]: Monad, R, A](r: R, a: A) extends Pipe[F, Any, R, A] {
  //   type Iterator = Option[A]
  //   def start: F[Option[A]] = (a.some).pure[F]

  //   def proceed(it: Iterator, f: Any) = (it match {
  //     case Some(a) => Next(Option.empty[A], a)
  //     case None    => End(r)
  //   }).pure[F]

  //   def stop(it: Iterator) = r.pure[F]
  // }

  // implicit def monad[F[_]: Atoms: Monad, I, R: Monoid]: Monad[Pipe[F, I, R, *]] = new Monad[Pipe[F, I, R, *]] {
  //   def flatMap[A, B](fa: Pipe[F, I, R, A])(f: A => Pipe[F, I, R, B]): Pipe[F, I, R, B] = ???

  //   def tailRecM[A, B](a: A)(f: A => Pipe[F, I, R, Either[A, B]]): Pipe[F, I, R, B] = ???

  //   def pure[A](x: A): Pipe[F, I, R, A] = ???
  // }
}
