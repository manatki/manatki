package manatki.free
import cats.~>

trait FunK[-F[_], +G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object FunK {
  def apply[F[_]] = new FunKApplied[F](true)

  def id[F[_]]: FunK[F, F] = identityAny.asInstanceOf[FunK[F, F]]

  abstract class FunKImpl[X, F[_], G[_]] extends FunK[F, G] {
    def applyA(fa: F[X]): G[X]

    def apply[A](fa: F[A]): G[A] = applyA(fa.asInstanceOf[F[X]]).asInstanceOf[G[A]]
  }

  class FunKApplied[F[_]](val dummy: Boolean) extends AnyVal {
    type Arbitrary
    def apply[G[_]](impl: FunKImpl[Arbitrary, F, G]): FunK[F, G] = impl
  }

  private val identityAny: FunK[Any, Any] = apply[Any](x => x)
}
