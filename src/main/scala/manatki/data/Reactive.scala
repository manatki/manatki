package manatki.data

import cats.arrow.{Compose, Profunctor}
import cats.data.Kleisli
import cats.free.Cofree
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Eval, FlatMap, Functor}

/**
  * something like Degoes Pipe
  * https://www.youtube.com/watch?v=wEVfJSi2J5o
*/
final case class Reactive[F[_], A, B](cell: Cofree[Kleisli[F, A, *], B]) {
  def current: B = cell.head
  def update(parent: A)(implicit F: Functor[F]): F[Reactive[F, A, B]] =
    cell.tail.map(_.run(parent).map(Reactive(_))).value

  def map[C](f: B => C)(implicit F: Functor[F]): Reactive[F, A, C] =
    Reactive(f(current), a => update(a).map(_.map(f)))

  def contramap[Z](f: Z => A)(implicit F: Functor[F]): Reactive[F, Z, B] =
    Reactive(current, a => update(f(a)).map(_.contramap(f)))

  def andThen[C](that: Reactive[F, B, C])(implicit F: FlatMap[F]): Reactive[F, A, C] =
    Reactive[F, A, C](
      that.current,
      (a: A) =>
        this.update(a).flatMap(a2b =>
          that.update(a2b.current).map(b2c =>
            a2b.andThen(b2c))))

  def compose[Z](that: Reactive[F, Z, A])(implicit F: FlatMap[F]): Reactive[F, Z, B] = that.andThen(this)

  def left[C](implicit F: Applicative[F]): Reactive[F, Either[A, C], Either[B, C]] = {
    def upd: Either[A, C] => F[Reactive[F, Either[A, C], Either[B, C]]] = {
      case Left(a) =>
        update(a).map(_.left[C])
      case Right(c) =>
        Reactive(c.asRight, upd).pure[F]
    }
    Reactive(current.asLeft, upd)
  }

  def merge[C, D](that: Reactive[F, C, D])(implicit F: Functor[F]): Reactive[F, Either[A, C], Either[B, D]] = {
    def upd: Either[A, C] => F[Reactive[F, Either[A, C], Either[B, D]]] = {
      case Left(a) =>
        this.update(a).map(_.merge(that))
      case Right(b) =>
        that.update(b).map(this.merge(_))
    }
    Reactive(current.asLeft, upd)
  }

}

object Reactive {
  def apply[F[_] : Functor, A, B](current: B, update: A => F[Reactive[F, A, B]]): Reactive[F, A, B] =
    Reactive(Cofree(current, Eval.now(Kleisli((a: A) => update(a).map(_.cell)))))

  def stateless[F[_] : Functor, A, B](current: B, update: A => F[B]): Reactive[F, A, B] =
    apply(current, a => update(a).map(b => stateless(b, update)))

  implicit def functorInstance[F[_] : Applicative, A]: Applicative[Reactive[F, A, *]] = new Applicative[Reactive[F, A, *]] {
    override def pure[B](x: B): Reactive[F, A, B] = {
      lazy val inst: Reactive[F, A, B] = Reactive(x, _ => inst.pure[F])
      inst
    }
    override def ap[B, C](ff: Reactive[F, A, B => C])(fb: Reactive[F, A, B]): Reactive[F, A, C] =
      Reactive(ff.current(fb.current), a => ff.update(a).map2(fb.update(a))(ap(_)(_)))

    override def map[B, C](fa: Reactive[F, A, B])(f: B => C): Reactive[F, A, C] = fa.map(f)

    override def map2[B, C, D](fa: Reactive[F, A, B], fb: Reactive[F, A, C])(f: (B, C) => D): Reactive[F, A, D] =
      Reactive[F, A, D](f(fa.current, fb.current), (a: A) => fa.update(a).map2(fb.update(a))(map2(_, _)(f)))
  }

  implicit def profunctorInstance[F[_] : Functor]: Profunctor[Reactive[F, *, *]] = new Profunctor[Reactive[F, *, *]] {
    override def dimap[A, B, C, D](fab: Reactive[F, A, B])(f: C => A)(g: B => D): Reactive[F, C, D] = fab.contramap(f).map(g)
  }

  implicit def composeInstance[F[_] : FlatMap]: Compose[Reactive[F, *, *]] = new Compose[Reactive[F, *, *]] {
    override def compose[A, B, C](f: Reactive[F, B, C], g: Reactive[F, A, B]): Reactive[F, A, C] = g.andThen(f)
  }
}
