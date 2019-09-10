package manatki.data.layer
import manatki.data.layer.LayerEffects._

abstract class Layer1[-P[-i[_], +o[_]], A] {
  type LP[a] <: Layer1[P, a]
  def unpack[R[_]](p: P[LP, R]): R[A]
}

object Layer1 {
  def apply[P[i[_], o[_]], A](impl: Impl[P, A]): Layer1[P, A] = impl
  def apply[P[i[_], o[_]]]                                    = new Applied[P]

  def pure[A](a: A): Layer1[PureP, A] = Pure(a)

  implicit class LayerOps[P[i[_], o[_]], A](private val l: Layer1[P, A]) extends AnyVal {
    def flatMap[B](f: A => Layer1[P, B]): Layer1[P with FlatMapP, B] = FlatMap[P, A, B](l, f)
  }

  implicit class EvalOps[A](private val ev: Layer1[EvalP, A]) extends AnyVal{
    def value: A = ev match {
        case Pure(a) => a
        case fm@FlatMap(_, _) =>  ???
        case e1 => 
            val res = e1.unpack[Eval](new EvalPI[Eval, Eval]{
                def pure[A](a: A): Eval[A] = Pure(a)
                def flatMap[A, B](fa: Eval[A], f: A => Eval[B]): Eval[B] = 
                    fa match {
                        case Pure(a) => f(a)
                        case e2 => e2.unpack[Eval](new EvalPI[Eval, Eval] {
                            def pure[A](a: A): LayerEffects.Eval[A] = ???
                            def flatMap[A, B](fa: LayerEffects.Eval[A], f: A => LayerEffects.Eval[B]): LayerEffects.Eval[B] = ???
                        })
                    }
            })
            res.value
    }
  }

  abstract class Impl[P[i[_], o[_]], A] extends Layer1[P, A] {
    final type LP[a] = Layer1[P, a]
    type Arb[a]

    def unpackArb(p: P[LP, Arb]): Arb[A]

    def unpack[R[_]](p: P[LP, R]): R[A] = unpackArb(p.asInstanceOf[P[LP, Arb]]).asInstanceOf[R[A]]
  }

  case class Pure[A](a: A) extends Layer1[PureP, A]{
      def unpack[R[_]](p: PureP[LP,R]): R[A] = p.pure(a)
  }

  final case class FlatMap[P[-i[_], +o[_]], A, B](
      start : Layer1[P, A],
      cont: A => Layer1[P, B]
  ) extends Layer1[P with FlatMapP, B]{
      type LP[a] = Layer1[P with FlatMapP, a]
      def unpack[R[_]](p: P[LP, R] with FlatMapP[LP, R]): R[B] = p.flatMap(start, cont)
  }

  class Applied[P[i[_], o[_]]] {
    type Arb[_]
    final type LP[a] = Layer1[P, a]
    def apply[A](f: P[LP, Arb] => Arb[A]): Layer1[P, A] = new Layer1[P, A] {
      type LP[a] = Layer1[P, a]
      def unpack[R[_]](p: P[LP, R]): R[A] = f(p.asInstanceOf[P[LP, Arb]]).asInstanceOf[R[A]]
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
  abstract class EvalPI[i[_], o[_]] extends PureP[i, o] with FlatMapP[i, o]

  type Eval[A] = Layer1[EvalP, A]
}
