package manatki.data.tagless

import cats.arrow.Profunctor

trait Layer[-P[-_, +_]] {
  def unpack[A](p: P[Layer[P], A]): A
}

object Layer {
  type IdC[+A] = A

  def apply[P[-_, +_]] = new Applied[P](true)
  def mk[P[-_, +_]]    = new Applied[P](true)

  class Applied[P[-_, +_]](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(maker: MakeLayer[P, Arb]): Layer[P] = maker
  }

  abstract class MakeLayer[P[-_, +_], Arb] extends Layer[P] {
    def applyArbitrary(fk: P[Layer[P], Arb]): Arb

    def unpack[A](fk: P[Layer[P], A]): A = applyArbitrary(fk.asInstanceOf[P[Layer[P], Arb]]).asInstanceOf[A]
  }

  implicit class LayerOps[P[-_, +_]](val layer: Layer[P]) extends AnyVal {
    def fold[A](p: P[A, A])(implicit P: Profunctor[P]): A = layer.unpack(P.lmap(p)(_.fold(p)))
  }

  def unfold[A, P[-_, +_]](builder: Builder[P, A])(init: A)(implicit P: Profunctor[P]): Layer[P] =
    new Layer[P] {
      def unpack[B](p: P[Layer[P], B]): B =
        builder.continue(init, P.lmap(p)(unfold(builder)))
    }
}

// aka coalgebra
trait Builder[-P[_, _], A] {
  def continue[B](init: A, p: P[A, B]): B
}
