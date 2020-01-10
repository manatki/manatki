package manatki.data.tagless

import cats.Eval
import cats.arrow.Profunctor
import tofu.syntax.monadic._

sealed trait LayerOr[-P[-_, +_], +A]

trait Layer[-P[-_, +_]] extends LayerOr[P, Nothing] {
  def unpack[A](p: P[Layer[P], A]): A
}

final case class LayerVal[+A](a: A) extends LayerOr[Any, A]

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

  implicit class LayerOps[P[-_, +_]](private val layer: Layer[P]) extends AnyVal {
    def fold[A](p: P[A, A])(implicit P: Profunctor[P]): A = layer.unpack(P.lmap(p)(_.fold(p)))

    def foldEval[A](p: P[Eval[A], Eval[A]])(implicit P: Profunctor[P]): Eval[A] =
      layer.unpack(P.lmap(p)(x => Eval.defer(x.foldEval(p))))

    def foldL[A](p: P[A, A])(implicit P: ProTraverse[P]): Eval[A] = layer.foldEval(P.protraverse(p))

    def para[A](p: P[(A, Layer[P]), A])(implicit P: ProCorepresentable[P]): A =
      layer.unpack(P.lmap(p)(la => (la.para(p), la)))

    def paraEval[A](p: P[(Eval[A], Layer[P]), Eval[A]])(implicit P: ProCorepresentable[P]): Eval[A] =
      layer.unpack(P.lmap(p)(la => (Eval.defer(la.paraEval(p)), la)))

    def paraL[A](p: P[(A, Layer[P]), A])(implicit P: ProTraverse[P]): Eval[A] = paraEvalT[P, A](layer, P.protraverse(p))
  }

  private def paraEvalT[P[-_, +_], A](l: Layer[P], p: P[Eval[(A, Layer[P])], Eval[A]])(
      implicit P: ProCorepresentable[P]
  ): Eval[A] =
    l.unpack(P.lmap(p)(la => Eval.defer(paraEvalT(la, p).tupleRight(la))))

  def unfold[A, P[-_, +_]](init: A)(builder: Builder[P, A])(implicit P: Profunctor[P]): Layer[P] =
    new Layer[P] {
      def unpack[B](p: P[Layer[P], B]): B =
        builder.continue(init, P.lmap(p)(unfold(_)(builder)))
    }

  def apo[A, P[-_, +_]](init: A)(builder: Builder[P, LayerOr[P, A]])(implicit P: Profunctor[P]): Layer[P] =
    new Layer[P] {
      def unpack[B](p: P[Layer[P], B]): B =
        builder.continue(LayerVal(init), P.lmap(p) {
          case l: Layer[P] => l
          case LayerVal(a) => apo(a)(builder)
        })
    }

  def hylo[A, B, P[_, _]](b: Builder[P, A])(p: P[B, B])(a: A)(implicit P: Profunctor[P]): B =
    b.continue(a, P.lmap(p)(hylo(b)(p)))

  def hyloEval[A, B, P[_, _]](
      b: Builder[P, A]
  )(p: P[Eval[B], Eval[B]])(a: A)(implicit P: Profunctor[P]): Eval[B] =
    b.continue(a, P.lmap(p)(ea => Eval.defer(hyloEval(b)(p)(ea))))

  def hyloL[A, B, P[_, _]](b: Builder[P, A])(p: P[B, B])(a: A)(implicit P: ProTraverse[P]): Eval[B] =
    hyloEval(b)(P.protraverse(p))(a)
}

// aka coalgebra
trait Builder[-P[_, _], A] {
  def continue[B](init: A, p: P[A, B]): B
}
