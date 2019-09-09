package manatki.data.layer
import manatki.data.layer.LayerEffects.PureP

abstract class Layer1[-P[-i[+_], +o[+_]], +A] {
  type LP[+a] <: Layer1[P, a]
  def unpack[R[+_]](p: P[LP, R]): R[A]
}

object Layer1 {
  def apply[P[-i[+_], +o[+_]], A](impl: Impl[P, A]) : Layer1[P, A] = impl

  def pure[A](a: A): Layer1[PureP, A] = apply[PureP, A](_.pure(a))

  abstract class Impl[P[-i[+_], +o[+_]], A] extends Layer1[P, A]{
    final type LP[+a] = Layer1[P, a]
    type Arb[+a]

    def unpackArb(p: P[LP, Arb]): Arb[A]

    def unpack[R[+_]](p: P[LP,R]): R[A] = unpackArb(p.asInstanceOf[P[LP, Arb]]).asInstanceOf[R[A]]
  }
}

object LayerEffects {
  trait PureP[-i[+_], +o[+_]] {
    def pure[A](a: A): o[A]
  }

  trait FlatMapP[-i[+_], +o[+_]] {
    def flatMap[A, B](fa: i[A], f: A => i[B]): o[B]
  }

  type EvalP[-i[+_], +o[+_]] = PureP[i, o] with FlatMapP[i, o]
}
