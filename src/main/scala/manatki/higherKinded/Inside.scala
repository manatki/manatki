package manatki.higherKinded

import cats.tagless.FunctorK
import cats.tagless.syntax.functorK._
import cats.{FlatMap, Functor, ~>}
import tofu.higherKind.{Embed, Point, PureK}
import tofu.syntax.monadic._

/** An equivalent of {{{F[U[F]]}}} allowing to overcome need of Functor[F] via co-Yoneda trick
  * */
abstract class Inside[U[_[_]], F[_]] {
  type A
  def pivot: F[A]
  def continue(a: A): U[F]

  def get(implicit F: Functor[F]): F[U[F]] = F.map(pivot)(continue)
}

object Inside {
  case class Impl[U[_[_]], F[_]](pivot: F[U[F]]) extends Inside[U, F] {
    type A = U[F]
    def continue(a: U[F]): U[F]                       = a
    override def get(implicit F: Functor[F]): F[U[F]] = pivot
  }

  def apply[U[_[_]], F[_]](fuf: F[U[F]]): U Inside F = Impl(fuf)

  implicit def embed[U[_[_]]]: Embed[U Inside *[_]]                 = embedAny.asInstanceOf[Embed[U Inside *[_]]]
  implicit def pureK[U[_[_]]]: PureK[U Inside *[_]]                 = pureKAny.asInstanceOf[PureK[U Inside *[_]]]
  implicit def functorK[U[_[_]]: FunctorK]: FunctorK[U Inside *[_]] = new FunctorKInstance

  class EmbedInstance[U[_[_]]] extends Embed[Inside[U, *[_]]] {
    def embed[F[_]: FlatMap](ft: F[Inside[U, F]]): Inside[U, F] = Inside(ft.flatMap(_.get))
  }
  private[this] val embedAny = new EmbedInstance[Any]

  class FunctorKInstance[U[_[_]]: FunctorK] extends FunctorK[Inside[U, *[_]]] {
    def mapK[F[_], G[_]](af: Inside[U, F])(fk: F ~> G): Inside[U, G] =
      new Inside[U, G] {
        type A = af.A
        val pivot                = fk(af.pivot)
        def continue(a: A): U[G] = af.continue(a).mapK(fk)
      }
  }

  class PureKInstance[U[_[_]]] extends PureK[Inside[U, *[_]]] {
    def pureK[F[_]](p: Point[F]): Inside[U, F] = Inside(p.point)
  }
  private[this] val pureKAny = new PureKInstance[Any]
}
