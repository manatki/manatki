package manatki.higherKinded

import cats.data.Tuple2K
import cats.tagless.{ApplyK, FunctorK}
import cats.{Apply, FlatMap, ~>}
import manatki.data.day.Day
import manatki.syntax.{funK, functionK}
import cats.syntax.flatMap._

trait Repr[T[_[_]], A] {
  def apply[F[_]](fk: T[F]): F[A]
}

object Repr {


  def apply[T[_[_]]] = new Applied[T]

  class Applied[T[_[_]]] {
    def apply[A](maker: MakeRepr[T, A, Arb]): Repr[T, A] = maker

    type Arb[_]
  }

  abstract class MakeRepr[T[_[_]], A, Arb[_]] extends Repr[T, A] {
    def applyArbitrary(fk: T[Arb]): Arb[A]

    def apply[F[_]](fk: T[F]): F[A] = applyArbitrary(fk.asInstanceOf[T[Arb]]).asInstanceOf[F[A]]
  }
}

trait RepresentableK[T[_[_]]] extends ApplyK[T] with Embed[T] {
  def tabulate[F[_]](hom: Repr[T, *] ~> F): T[F]

  def mapK[F[_], G[_]](af: T[F])(fk: F ~> G): T[G] = tabulate(funK(repr => fk(repr(af))))

  def productK[F[_], G[_]](af: T[F], ag: T[G]): T[Tuple2K[F, G, *]] =
    tabulate(funK(repr => Tuple2K(repr(af), repr(ag))))

  def embed[F[_]: FlatMap](ft: F[T[F]]): T[F] = tabulate(funK(repr => ft.flatMap(repr(_))))
}

object RepresentableK {
  def index[T[_[_]], F[_], A](tf: T[F])(repr: Repr[T, A]): F[A] = repr(tf)
}
