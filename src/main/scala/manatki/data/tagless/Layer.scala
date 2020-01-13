package manatki.data.tagless

import cats.{Comonad, Eval, Monad, StackSafeMonad}
import cats.arrow.Profunctor
import manatki.data.tagless.Rep.Pro
import tofu.higherKind.Function2K
import tofu.syntax.monadic._

sealed trait LayerOr[-P[-_, +_], +A]

trait Layer[-P[-_, +_]] extends LayerOr[P, Nothing] {
  def unpack[R](p: P[Layer[P], R]): R
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

    def unpack[R](fk: P[Layer[P], R]): R = applyArbitrary(fk.asInstanceOf[P[Layer[P], Arb]]).asInstanceOf[R]
  }

  implicit class LayerOps[P[-_, +_]](private val layer: Layer[P]) extends AnyVal {
    def contramapK2[Q[-_, +_]](f: FunK2[Q, P])(implicit Q: Profunctor[Q]): Layer[Q] = new Layer[Q] {
      def unpack[A](q: Q[Layer[Q], A]): A =
        layer.unpack[A](f(Q.lmap(q)((_: Layer[P]).contramapK2(f))))
    }

    def fold[A](p: P[A, A])(implicit P: Profunctor[P]): A = layer.unpack(P.lmap(p)(_.fold(p)))

    def foldEval[A](p: P[Eval[A], Eval[A]])(implicit P: Profunctor[P]): Eval[A] =
      layer.unpack(P.lmap(p)(x => Eval.defer(x.foldEval(p))))

    def foldL[A](p: P[A, A])(implicit P: ProTraverse[P]): Eval[A] = layer.foldEval(P.protraverse(p))

    def para[A](p: P[(A, Layer[P]), A])(implicit P: ProCorepresentable[P]): A =
      layer.unpack(P.lmap(p)(la => (la.para(p), la)))

    def paraEval[A](p: P[(Eval[A], Layer[P]), Eval[A]])(implicit P: ProCorepresentable[P]): Eval[A] =
      layer.unpack(P.lmap(p)(la => (Eval.defer(la.paraEval(p)), la)))

    def paraL[A](p: P[(A, Layer[P]), A])(implicit P: ProTraverse[P]): Eval[A] = paraEvalT[P, A](layer, P.protraverse(p))

    def prepro[A](f: FunK2[P, P])(p: P[A, A])(implicit P: Profunctor[P]): A = layer.unpack(f(P.lmap(p)(_.prepro(f)(p))))

    def preproEval[A](f: FunK2[P, P])(p: P[Eval[A], Eval[A]])(implicit P: Profunctor[P]): Eval[A] =
      layer.unpack(f(P.lmap(p)(x => Eval.defer(x.preproEval(f)(p)))))

    def preproL[A](f: FunK2[P, P])(p: P[A, A])(implicit P: ProTraverse[P]): Eval[A] =
      layer.preproEval(f)(P.protraverse(p))
  }

  private def paraEvalT[P[-_, +_], A](l: Layer[P], p: P[Eval[(A, Layer[P])], Eval[A]])(
      implicit P: ProCorepresentable[P]
  ): Eval[A] =
    l.unpack(P.lmap(p)(la => Eval.defer(paraEvalT(la, p).tupleRight(la))))

}

// aka coalgebra
trait Builder[-P[_, _], A] {
  def continue[R](init: A, p: P[A, R]): R
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

    def postpro(f: FunK2[P, P])(init: A)(implicit P: Profunctor[P]): Layer[P] = new Layer[P] {
      def unpack[B](p: P[Layer[P], B]): B =
        builder.continue(init, f(P.lmap(p)(postpro(f)(_))))
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

trait CofreeP[-P[-_, +_], +A] {
  def value: A
  def unpack[R](p: P[CofreeP[P, A], R]): R
}

object CofreeP {
  def apply[P[-_, +_]] = new Applied[P](true)
  def mk[P[-_, +_]]    = new Applied[P](true)

  class Applied[P[-_, +_]](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply[A](a: A)(maker: MakeLayer[A, P, Arb]): CofreeP[P, A] = new CofreeP[P, A] {
      def value: A                             = a
      def unpack[R](p: P[CofreeP[P, A], R]): R = maker.unpack(p)
    }
  }

  abstract class MakeLayer[A, P[-_, +_], Arb] {
    def applyArbitrary(fk: P[CofreeP[P, A], Arb]): Arb

    def unpack[R](fk: P[CofreeP[P, A], R]): R = applyArbitrary(fk.asInstanceOf[P[CofreeP[P, A], Arb]]).asInstanceOf[R]
  }

  implicit def cofreeInstance[P[-_, +_]](implicit P: Profunctor[P]): Comonad[CofreeP[P, *]] =
    new Comonad[CofreeP[P, *]] {
      def extract[A](x: CofreeP[P, A]): A = x.value
      def coflatMap[A, B](fa: CofreeP[P, A])(f: CofreeP[P, A] => B): CofreeP[P, B] =
        new CofreeP[P, B] {
          def value: B = f(fa)
          def unpack[R](p: P[CofreeP[P, B], R]): R =
            fa.unpack(P.lmap(p)(coflatMap(_)(f)))
        }
      def map[A, B](fa: CofreeP[P, A])(f: A => B): CofreeP[P, B] =
        new CofreeP[P, B] {
          def value: B = f(fa.value)
          def unpack[R](p: P[CofreeP[P, B], R]): R =
            fa.unpack(P.lmap(p)(map(_)(f)))
        }
    }
}

trait FreeP[-P[-_, +_], +A] {
  def unpack[R](pf: P[FreeP[P, A], R])(ar: A => R): R
}

object FreeP {
  def apply[P[-_, +_], A] = new Applied[P, A](true)
  def mk[P[-_, +_], A]    = new Applied[P, A](true)

  def pure[A](a: A): FreeP[Any, A] = new FreeP[Any, A] {
    def unpack[R](pf: Any)(ar: A => R): R = ar(a)
  }

  class Applied[P[-_, +_], A](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(maker: MakeLayer[P, A, Arb]): FreeP[P, A] = maker
  }

  abstract class MakeLayer[P[-_, +_], A, Arb] extends FreeP[P, A] {
    def applyArbitrary(fk: P[FreeP[P, A], Arb]): Arb

    def unpack[R](fk: P[FreeP[P, A], R])(ar: A => R): R =
      applyArbitrary(fk.asInstanceOf[P[FreeP[P, A], Arb]]).asInstanceOf[R]
  }

  implicit def freeMonad[P[-_, +_]](implicit P: Profunctor[P]): Monad[FreeP[P, *]] =
    new StackSafeMonad[FreeP[P, *]] {
      def flatMap[A, B](fa: FreeP[P, A])(f: A => FreeP[P, B]): FreeP[P, B] = new FreeP[P, B] {
        def unpack[R](pf: P[FreeP[P, B], R])(br: B => R): R =
          fa.unpack(P.lmap(pf)(flatMap(_)(f)))(f(_).unpack(pf)(br))
      }
      def pure[A](x: A): FreeP[P, A] = FreeP.pure(x)
    }
}

trait FreerP[-P[-_, +_], +A] {
  def unpack[R](pf: FreerP.Bind[P, A, R])(ar: A => R): R
}

object FreerP {
  trait Bind[+P[-_, +_], -B, R] {
    def continue[A](pa: Rep.Pro[P, A])(k: A => FreerP[P, B]): R
  }

  def pure[A](x: A): FreerP[Any, A] = new FreerP[Any, A] {
    def unpack[R](pf: Bind[Any, A, R])(ar: A => R): R = ar(x)
  }

  implicit def freerMonad[P[-_, +_]](implicit P: Profunctor[P]): Monad[FreerP[P, *]] =
    new StackSafeMonad[FreerP[P, *]] {
      def flatMap[A, B](fa: FreerP[P, A])(f: A => FreerP[P, B]): FreerP[P, B] =
        fa.unpack(new Bind[P, A, FreerP[P, B]] {
          def continue[C](pa: Pro[P, C])(k: C => FreerP[P, A]): FreerP[P, B] = new FreerP[P, B] {
            def unpack[R](pf: Bind[P, B, R])(ar: B => R): R = pf.continue(pa)(c => k(c).flatMap(f))
          }
        })(a => f(a))

      def pure[A](x: A): FreerP[P, A] = FreerP.pure(x)
    }
}
