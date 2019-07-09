package manatki.data.layer

import cats.{Applicative, Eval, Id}
import cats.arrow.Profunctor
import manatki.control.Delay
import manatki.data.layer.Layer.IdC


trait Layer[-P[- _, + _]] {
  def unpackF[F[+ _]: Applicative, A](p: P[F[Layer[P]], F[A]]): F[A]

  def unpack[A](p: P[Layer[P], A]): A = unpackF[IdC, A](p)

  def unpackLazy[A](p: P[Eval[Layer[P]], Eval[A]]): Eval[A] = unpackF[Eval, A](p)
}

object Layer {
  type IdC[+A] = A

  implicit class LayerOps[P[- _, + _]](val layer: Layer[P]) extends AnyVal {
    def capture(implicit P: Profunctor[P]): Capture[P] = new Capture[P] {
      def foldD[A](k: P[A, A])(implicit d: Delay[A]): A =
        layer.unpack[A](P.lmap(k)((lp: Layer[P]) => d.delay(lp.capture.foldD(k))))
    }
  }
}


trait Capture[-P[- _, + _]] {
  def foldD[A: Delay](k: P[A, A]): A

  def fold[A](k: P[A, A]): A = foldD[A](k)(Delay.force)
  def foldLazy[A](k: P[Eval[A], Eval[A]]): Eval[A] = foldD(k)
}

// aka coalgebra
trait Builder[-P[- _, + _], A] {
  def continue[B](init: A, p: P[A, B]): B
}

trait Layered[P[- _, + _]] extends Profunctor[P] {
  def layer: P[Layer[P], Layer[P]]
  def capture: P[Capture[P], Capture[P]]
}

object Layered {
  def build[P[- _, + _], A](init: A, b: Builder[P, A])(implicit l: Layered[P]): Layer[P] =
    b.continue(init, l.lmap(l.layer)(build(_, b)))
}

trait Recursive1[-P[- _[_], + _[_]], A] {
  def continue[F[_]](k: P[F, F]): F[A]
}
