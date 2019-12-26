package manatki.data.tagless

import cats.Eval
import cats.arrow.Profunctor
import manatki.control.Delay

trait Layer[-P[-_, +_]] {
  def unpack[A](p: P[Layer[P], A]): A
}

object Layer {
  type IdC[+A] = A

  implicit class LayerOps[P[-_, +_]](val layer: Layer[P]) extends AnyVal {
    def capture(implicit P: Profunctor[P]): Capture[P] = new Capture[P] {
      def foldD[A](k: P[A, A])(implicit d: Delay[A]): A =
        layer.unpack[A](P.lmap(k)((lp: Layer[P]) => d.delay(lp.capture.foldD(k))))
    }
  }
}

trait Capture[-P[-_, +_]] {
  def foldD[A: Delay](k: P[A, A]): A

  def fold[A](k: P[A, A]): A                       = foldD[A](k)(Delay.force)
  def foldLazy[A](k: P[Eval[A], Eval[A]]): Eval[A] = foldD(k)
}

// aka coalgebra
trait Builder[-P[-_, +_], A] {
  def continue[B](init: A, p: P[A, B]): B
}

trait Layered[P[-_, +_]] extends Profunctor[P] {
  def layer: P[Layer[P], Layer[P]]
  def capture: P[Capture[P], Capture[P]]
}

object Layered {
  def build[P[-_, +_], A](init: A, b: Builder[P, A])(implicit l: Layered[P]): Layer[P] =
    b.continue(init, l.lmap(l.layer)(build(_, b)))
}
