package manatki.syntax

import cats.arrow.FunctionK
import cats.~>


// credit to Alex Konovalov for this trick: undefined type members
// with unstable prefixes, to encode universal quantification
private[syntax] final class MkFunctionK[F[_]](val dummy: Boolean = true) extends AnyVal {
  type T

  def apply[G[_]](f: F[T] => G[T]): FunctionK[F, G] = new FunctionK[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa.asInstanceOf[F[T]]).asInstanceOf[G[A]]
  }
}

object functionK {
  def apply[F[_]]: MkFunctionK[F] = new MkFunctionK
}


object funK {
  def apply[F[_], G[_]](maker: MakeFunctionK[F, G]): F ~> G = maker

  abstract class MakeFunctionK[F[_], G[_]] extends (F ~> G) {

    def applyArbitrary(f: F[Arb]): G[Arb]

    def apply[A](fa: F[A]): G[A] = applyArbitrary(fa.asInstanceOf[F[Arb]]).asInstanceOf[G[A]]
  }
  type Arb
}
