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

}

// aka coalgebra
trait Builder[-P[_, _], A] {
  def continue[B](init: A, p: P[A, B]): B
}

object Builder {

  def apply[P[_, _], A] = new Applied[P, A](true)

  class Applied[P[_, _], A](private val __ : Boolean) extends AnyVal {
    type Arb

    def apply(builder: ArbBuilder[P, A, Arb]): Builder[P, A] = builder
  }

  abstract class ArbBuilder[P[_, _], A, W] extends Builder[P, A] {
    def continueArb(init: A, p: P[A, W]): W

    def continue[B](init: A, p: P[A, B]): B = continueArb(init, p.asInstanceOf[P[A, W]]).asInstanceOf[B]
  }

  implicit class BuilderOps[P[_, _], A](private val builder: Builder[P, A]) extends AnyVal {
    def hylo[B](p: P[B, B])(a: A)(implicit P: Profunctor[P]): B = builder.continue(a, P.lmap(p)(hylo(p)))

    def hyloEval[B](p: P[Eval[B], Eval[B]])(a: A)(implicit P: Profunctor[P]): Eval[B] =
      builder.continue(a, P.lmap(p)(ea => Eval.defer(hyloEval(p)(ea))))

    def hyloL[B](p: P[B, B])(a: A)(implicit P: ProTraverse[P]): Eval[B] = hyloEval(P.protraverse(p))(a)
  }

  implicit class BuilderUnfoldOps[P[-_, +_], A](private val builder: Builder[P, A]) extends AnyVal {
    def unfold(init: A)(implicit P: Profunctor[P]): Layer[P] =
      new Layer[P] {
        def unpack[B](p: P[Layer[P], B]): B =
          builder.continue(init, P.lmap(p)(unfold(_)))
      }
  }

  implicit class BuilderApoOps[P[-_, +_], A](private val builder: Builder[P, LayerOr[P, A]]) extends AnyVal {
    def apo(init: A)(implicit P: Profunctor[P]): Layer[P] =
      new Layer[P] {
        def unpack[B](p: P[Layer[P], B]): B =
          builder.continue(LayerVal(init), P.lmap(p) {
            case l: Layer[P] => l
            case LayerVal(a) => apo(a)
          })
      }
  }

}
