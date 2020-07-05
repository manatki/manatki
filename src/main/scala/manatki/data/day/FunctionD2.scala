package manatki.data.day
import cats.{Eval, ~>}
import cats.data.Tuple2K
import tofu.syntax.funk.funK

trait FunctionD2[F[_], G[_], H[_]] {
  def apply[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => Eval[C]): H[C]

  def tupled: Day[F, G, *] ~> H = funK(dfg => apply(dfg.fx, dfg.gy)(dfg.comb))
}

object FunctionD2 {
  def apply[F[_], G[_]] = new Make[F, G](true)

  class Make[F[_], G[_]](private val __ : Boolean) extends AnyVal {
    type ArbA
    type ArbB
    type ArbC
    def apply[H[_]](maker: Maker[F, G, H, ArbA, ArbB, ArbC]): FunctionD2[F, G, H] = maker
  }

  abstract class Maker[F[_], G[_], H[_], a, b, c] extends FunctionD2[F, G, H] {
    def applyArb(fa: F[a], gb: G[b], f: (a, b) => Eval[c]): H[c]

    final def apply[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => Eval[C]): H[C] =
      applyArb(fa.asInstanceOf[F[a]], gb.asInstanceOf[G[b]], f.asInstanceOf[(a, b) => Eval[c]]).asInstanceOf[H[C]]
  }
}
