package manatki.data.tagless

import cats.syntax.coflatMap._
import cats.syntax.comonad._
import cats.{Comonad, Eval, Monad, StackSafeMonad}
import manatki.data.tagless.PTrans.PTag
import tofu.syntax.monadic._
import cats.syntax.either._

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
    def contramapK2[Q[-_, +_]](f: FunK2[Q, P])(implicit Q: Pro[Q]): Layer[Q] = new Layer[Q] {
      def unpack[A](q: Q[Layer[Q], A]): A =
        layer.unpack[A](f(Q.lmap(q)((_: Layer[P]).contramapK2(f))))
    }

    def fold[A](p: P[A, A])(implicit P: Pro[P]): A = layer.unpack(P.lmap(p)(_.fold(p)))

    def gfold[W[_]: Comonad, A](dist: PDistr[P, W])(p: P[W[A], A])(implicit P: Pro[P]): A = {
      def go: P[Layer[P], W[Rep[P[W[A], *]]]] = P.lmap(dist.apply[W[A]])(_.unpack(go).map(_(p)).coflatten)
      layer.unpack(go).extract.apply(p)
    }

    def foldEval[A](p: P[Eval[A], Eval[A]])(implicit P: Pro[P]): Eval[A] =
      layer.unpack(P.lmap(p)(x => Eval.defer(x.foldEval(p))))

    def foldL[A](p: P[A, A])(implicit P: ProTraverse[P]): Eval[A] = layer.foldEval(P.protraverse(p))

    def para[A](p: P[(A, Layer[P]), A])(implicit P: ProCorep[P]): A =
      layer.unpack(P.lmap(p)(la => (la.para(p), la)))

    def paraEval[A](p: P[(Eval[A], Layer[P]), Eval[A]])(implicit P: ProCorep[P]): Eval[A] =
      layer.unpack(P.lmap(p)(la => (Eval.defer(la.paraEval(p)), la)))

    def paraL[A](p: P[(A, Layer[P]), A])(implicit P: ProTraverse[P]): Eval[A] = paraEvalT[P, A](layer, P.protraverse(p))

    def prepro[A](f: FunK2[P, P])(p: P[A, A])(implicit P: Pro[P]): A = layer.unpack(f(P.lmap(p)(_.prepro(f)(p))))

    def preproEval[A](f: FunK2[P, P])(p: P[Eval[A], Eval[A]])(implicit P: Pro[P]): Eval[A] =
      layer.unpack(f(P.lmap(p)(x => Eval.defer(x.preproEval(f)(p)))))

    def preproL[A](f: FunK2[P, P])(p: P[A, A])(implicit P: ProTraverse[P]): Eval[A] =
      layer.preproEval(f)(P.protraverse(p))

    def histo[A](p: P[CofreeP[P, A], A])(implicit P: ProCorep[P]): A = gfold(cofreeDist)(p)
  }

  private def cofreeDist[P[-_, +_]](implicit P: ProCorep[P]): PDistr[P, CofreeP[P, *]] = {
    def pdist[A]: P[CofreeP[P, A], CofreeP[P, Rep[P[A, *]]]] =
      P.tabulate { rep =>
        new CofreeP[P, Rep[P[A, *]]] {
          def value: Rep[P[A, *]] = rep.pmap(_.value)
          def unpack[R](p: P[CofreeP[P, Rep[P[A, *]]], R]): R =
            rep(P.lmap(p)(_.unpack(pdist)))
        }
      }
    PDistr[P, CofreeP[P, *]](pdist)
  }

  private def paraEvalT[P[-_, +_], A](l: Layer[P], p: P[Eval[(A, Layer[P])], Eval[A]])(
      implicit P: ProCorep[P]
  ): Eval[A] =
    l.unpack(P.lmap(p)(la => Eval.defer(paraEvalT(la, p).tupleRight(la))))

}

// aka coalgebra
trait Builder[-P[-_, +_], A] {
  def continue[R](init: A, p: P[A, R]): R
}

object Builder {

  def apply[P[-_, +_], A] = new Applied[P, A](true)

  class Applied[P[-_, +_], A](private val __ : Boolean) extends AnyVal {
    type Arb

    def apply(builder: ArbBuilder[P, A, Arb]): Builder[P, A] = builder
  }

  abstract class ArbBuilder[P[-_, +_], A, W] extends Builder[P, A] {
    def continueArb(init: A, p: P[A, W]): W

    def continue[B](init: A, p: P[A, B]): B = continueArb(init, p.asInstanceOf[P[A, W]]).asInstanceOf[B]
  }

  implicit class BuilderUnfoldOps[P[-_, +_], A](private val builder: Builder[P, A]) extends AnyVal {
    def unfold(init: A)(implicit P: Pro[P]): Layer[P] =
      new Layer[P] {
        def unpack[B](p: P[Layer[P], B]): B =
          builder.continue(init, P.lmap(p)(unfold(_)))
      }

//    def gunfold[M[_]: Monad]()(init: A)

    def postpro(f: FunK2[P, P])(init: A)(implicit P: Pro[P]): Layer[P] = new Layer[P] {
      def unpack[B](p: P[Layer[P], B]): B =
        builder.continue(init, f(P.lmap(p)(postpro(f)(_))))
    }

    def hylo[B](p: P[B, B])(a: A)(implicit P: Pro[P]): B = builder.continue(a, P.lmap(p)(hylo(p)))

    def hyloEval[B](p: P[Eval[B], Eval[B]])(a: A)(implicit P: Pro[P]): Eval[B] =
      builder.continue(a, P.lmap(p)(ea => Eval.defer(hyloEval(p)(ea))))

    def hyloL[B](p: P[B, B])(a: A)(implicit P: ProTraverse[P]): Eval[B] = hyloEval(P.protraverse(p))(a)
  }

  implicit class BuilderApoOps[P[-_, +_], A](private val builder: Builder[P, LayerOr[P, A]]) extends AnyVal {
    def apo(init: A)(implicit P: Pro[P]): Layer[P] =
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

  implicit def cofreeInstance[P[-_, +_]](implicit P: Pro[P]): Comonad[CofreeP[P, *]] =
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

  implicit class FreeMonadOps[P[-_, +_], A](private val self: FreeP[P, A]) extends AnyVal {
    def foldMap[M[+_]: Monad](f: PTrans[P, M])(implicit P: Pro[P]): M[A] =
      self.tailRecM[M, A](_.unpack(P.rmap(f[FreeP[P, A]])(_.map(_.asLeft[A])))(a => a.asRight.pure[M]))
  }

  abstract class MakeLayer[P[-_, +_], A, Arb] extends FreeP[P, A] {
    def applyArbitrary(fk: P[FreeP[P, A], Arb]): Arb

    def unpack[R](fk: P[FreeP[P, A], R])(ar: A => R): R =
      applyArbitrary(fk.asInstanceOf[P[FreeP[P, A], Arb]]).asInstanceOf[R]
  }

  implicit def freeMonad[P[-_, +_]](implicit P: Pro[P]): Monad[FreeP[P, *]] =
    new StackSafeMonad[FreeP[P, *]] {
      def flatMap[A, B](fa: FreeP[P, A])(f: A => FreeP[P, B]): FreeP[P, B] = new FreeP[P, B] {
        def unpack[R](pf: P[FreeP[P, B], R])(br: B => R): R =
          fa.unpack(P.lmap(pf)(flatMap(_)(f)))(f(_).unpack(pf)(br))
      }
      def pure[A](x: A): FreeP[P, A] = FreeP.pure(x)
    }
}

sealed trait FreerP[-P[-_, +_], +A] {
  def flatMap[Q[-i, +o] <: P[i, o], B](f: A => FreerP[Q, B]): FreerP[Q, B]
}

object FreerP {
  final case class Pure[+A](a: A) extends FreerP[Any, A] {
    def flatMap[Q[-i, +o] <: Any, B](f: A => FreerP[Q, B]): FreerP[Q, B] = f(a)
  }

  abstract class Bind[-P[-_, +_], X, +A] extends FreerP[P, A] { self =>
    def rep[R](pr: P[X, R]): R
    def continue(pin: X): FreerP[P, A]
    def flatMap[Q[-i, +o] <: P[i, o], B](f: A => FreerP[Q, B]): Bind[Q, X, B] =
      new Bind[Q, X, B] {
        def rep[R](pr: Q[X, R]): R         = self.rep(pr)
        def continue(pin: X): FreerP[Q, B] = self.continue(pin).flatMap(f)
        override def flatMap[U[-i, +o] <: Q[i, o], C](g: B => FreerP[U, C]): Bind[U, X, C] =
          self.flatMap(a => f(a).flatMap(g))
      }

  }

  def pure[A](x: A): FreerP[Any, A] = Pure(x)

  implicit def freerMonad[P[-_, +_]]: Monad[FreerP[P, *]] =
    new StackSafeMonad[FreerP[P, *]] {

      def flatMap[A, B](fa: FreerP[P, A])(f: A => FreerP[P, B]): FreerP[P, B] = fa.flatMap(f)
      def pure[A](x: A): FreerP[P, A]                                         = Pure(x)
    }
}

trait CofreerP[-P[-_, +_], +A] { self =>
  type Pin
  def value: A
  def rep[R](pr: P[Pin, R]): R
  def continue(pin: Pin): CofreerP[P, A]

  def coflatMap[B](f: CofreerP[P, A] => B): CofreerP[P, B] = new CofreerP[P, B] { inner =>
    type Pin = self.Pin
    def value: B                           = f(self)
    def rep[R](pr: P[Pin, R]): R           = self.rep(pr)
    def continue(pin: Pin): CofreerP[P, B] = self.continue(pin).coflatMap(f)

    override def coflatMap[C](g: CofreerP[P, B] => C): CofreerP[P, C] = self.coflatMap(fa => g(fa.coflatMap(f)))
  }

  def map[B](f: A => B): CofreerP[P, B] = coflatMap(fa => f(fa.value))
}

object CofreerP {
  implicit def cofreerComonad[P[-_, +_]]: Comonad[CofreerP[P, *]] =
    new Comonad[CofreerP[P, *]] {
      def extract[A](x: CofreerP[P, A]): A                                            = x.value
      def coflatMap[A, B](fa: CofreerP[P, A])(f: CofreerP[P, A] => B): CofreerP[P, B] = fa.coflatMap(f)
      def map[A, B](fa: CofreerP[P, A])(f: A => B): CofreerP[P, B]                    = fa.map(f)
    }
}

object PTrans {
  type T[+P[-_, +_], +F[_]] <: PTag

  trait PTag extends Any

  def apply[P[-_, +_], F[_]] = new Make[P, F](true)

  class Make[P[-_, +_], F[_]](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(p: P[Arb, F[Arb]]): T[P, F] = p.asInstanceOf[T[P, F]]
  }

  implicit class PTransOps[P[-_, +_], F[_]](private val t: T[P, F]) extends AnyVal {
    def apply[A]: P[A, F[A]] = t.asInstanceOf[P[A, F[A]]]
  }
}

object PDistr {
  type T[P[-_, +_], F[_]] <: PTag

  def apply[P[-_, +_], F[_]] = new Make[P, F](true)

  class Make[P[-_, +_], F[_]](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(p: P[F[Arb], F[Rep[P[Arb, *]]]]): T[P, F] = p.asInstanceOf[T[P, F]]
  }

  implicit class PDistrOps[P[-_, +_], F[_]](private val t: T[P, F]) extends AnyVal {
    def apply[A]: P[F[A], F[Rep[P[A, *]]]] = t.asInstanceOf[P[F[A], F[Rep[P[A, *]]]]]
  }
}

trait PCodistr[P[-_, +_], F[_]] {
  def apply[A, R](mr: F[Rep[P[A, *]]], pr: P[F[A], R]): R
}

object PCodistr {
  def apply[P[-_, +_], F[_]] = new Make[P, F](true)

  class Make[P[-_, +_], F[_]](private val __ : Boolean) extends AnyVal {
    type Arb1
    type Arb2

    def apply(maker: Maker[P, F, Arb1, Arb2]): PCodistr[P, F] = maker
  }

  trait Maker[P[-_, +_], F[_], AA, AR] extends PCodistr[P, F] {
    def applyArb(mr: F[Rep[P[AA, *]]], pr: P[F[AA], AR]): AR

    def apply[A, R](mr: F[Rep[P[A, *]]], pr: P[F[A], R]): R =
      applyArb(mr.asInstanceOf[F[Rep[P[AA, *]]]], pr.asInstanceOf[P[F[AA], AR]]).asInstanceOf[R]
  }
}
