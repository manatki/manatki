package manatki.data.tagless

import LayerEffects._
import cats.evidence.As

import scala.annotation.tailrec


sealed abstract class AsP[-P[-i[_], +o[_]], +Q[-i[_], +o[_]]]{
  def cosubst[F[-p[-i[_], +o[_]]]](fq: F[Q]): F[P]
  def subst[F[+p[-i[_], +o[_]]]](fp: F[P]): F[Q]
}

object AsP{
  private class Refl[P[-i[_], +o[_]]] extends AsP[P, P]{
    def cosubst[F[-p[-i[_], +o[_]]]](fq: F[P]): F[P] = fq
    def subst[F[+p[-i[_], +o[_]]]](fp: F[P]): F[P] = fp
  }

  private val reflAny : Refl[Any] = new Refl

  def refl[P[-i[_], +o[_]]]: AsP[P, P] = reflAny.asInstanceOf[AsP[P, P]]
  def reflSub[P[-i[_], +o[_]], Q[-i[_], +o[_]] >: P[i, o]]: AsP[P, Q] = refl[P]
}

trait Kneet[-P[-i[_], +o[_]], -L[-i[_], +o[_]], A] {
  type P1[-i[_], +o[_]] >: P[i, o]
  type L1[-i[_], +o[_]] >: L[i, o]
  type KPL[a] = Kneet[P1, L1, a]

  def asP: AsP[P, P1] = AsP.reflSub[P, P1]
  def asL: AsP[L, L1] = AsP.reflSub[L, L1]

  def unpack2[R[_]](p: P[KPL, R], q: L[KPL, R]): R[A]

  def run[R[_]](p: P[KPL, R])(implicit ev: Any <:< L[KPL, R]): R[A] = unpack2(p, ())
}

object Platform {
  def apply[P[i[_], o[_]], A](impl: Impl[P, A]): Platform[P, A] = impl
  def apply[P[i[_], o[_]]]                                      = new Applied[P]

  def pure[A](a: A): Platform[PureP, A] = Pure(a)

  implicit class LayerOps[P[i[_], o[_]], A](private val l: Platform[P, A]) extends AnyVal {
    type FM[i[_], o[_]] = P[i, o] with FlatMapP[i, o]
    def flatMap[B](f: A => Platform[P, B]): Platform[FM, B] = FlatMap[FM, A, B](l, f)
  }

  private object ValueEval extends EvalPI[Eval, Eval] {
    def pure[A](a: A): Eval[A] = Pure(a)
    def flatMap[A, B](fx: Eval[A], f: A => Eval[B]): Eval[B] =
      fx match {
        case Pure(a) => f(a)
        case _ =>
          type F[+p[-i[_], +o[_]]] = EvalP[Kneet[fx.P1, p, *], EvalCont[B, *]]
          fx.run(fx.asL.subst[F](new ValueFlatMapEval[B])).apply(f)
      }
  }

  private type EvalCont[R, A] = (A => Eval[R]) => Eval[R]

  private class ValueFlatMapEval[R] extends EvalPI[Eval, EvalCont[R, *]] {
    def pure[A](a: A) = f => f(a)
    def flatMap[A, B](fu: LayerEffects.Eval[A], g: A => LayerEffects.Eval[B]) =
      f => FlatMap[EvalP, A, R](fu, u => FlatMap(g(u), f))
  }

  implicit class EvalOps[A](private val ev: Platform[EvalP, A]) extends AnyVal {
    @tailrec def value: A = ev match {
      case Pure(a)        => a
      case FlatMap(fx, f) => f(fx).value
      case e1             =>
        type F[+p[-i[_], +o[_]]] = EvalP[Kneet[e1.P1, p, *], Eval]
        e1.run(e1.asL.subst[F](ValueEval)).value
    }
  }

  abstract class Impl[P[-i[_], +o[_]], A] extends Platform[P, A] {
    final type P1[-i[_], +o[_]] = P[i, o]
    type Arb[a]

    def unpackArb(p: P[KPL, Arb]): Arb[A]

    def unpack[R[_]](p: P[KPL, R]): R[A] = unpackArb(p.asInstanceOf[P[KPL, Arb]]).asInstanceOf[R[A]]
  }

  case class Pure[A](a: A) extends Platform[PureP, A] {
    def unpack2[R[_]](p: PureP[KPL, R], q: Any): R[A] = p.pure(a)
  }

  final case class FlatMap[P[-i[_], +o[_]] <: FlatMapP[i, o], A, B](
      start: Platform[P, A],
      cont: A => Platform[P, B]
  ) extends Platform[P, B] {
    type P1[-i[_], +o[_]] = P[i, o]
    def unpack2[R[_]](p: P[KPL, R], q: Any): R[B] = p.flatMap(start, cont)
  }

  class Applied[P[-i[_], +o[_]]] {
    type Arb[_]
    final type LP[a] = Platform[P, a]
    def apply[A](f: P[LP, Arb] => Arb[A]): Platform[P, A] = new Platform[P, A] {
      type P1[-i[_], +o[_]] = P[i, o]
      def unpack2[R[_]](p: P[KPL, R], q: Any): R[A] = f(p.asInstanceOf[P[LP, Arb]]).asInstanceOf[R[A]]
    }
  }
}

object LayerEffects {
  trait PureP[-i[_], +o[_]] {
    def pure[A](a: A): o[A]
  }

  trait FlatMapP[-i[_], +o[_]] {
    def flatMap[A, B](fa: i[A], f: A => i[B]): o[B]
  }

  type EvalP[-i[_], +o[_]] = PureP[i, o] with FlatMapP[i, o]
  abstract class EvalPI[-i[_], +o[_]] extends PureP[i, o] with FlatMapP[i, o]
  def lol[i[_], o[_]] = implicitly[EvalPI[i, o] <:< EvalP[i, o]]

  type Eval[A] = Platform[EvalP, A]
}
