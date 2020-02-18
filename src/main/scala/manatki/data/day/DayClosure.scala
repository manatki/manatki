package manatki.data.day
import cats.{Eval, Functor, Id, ~>}
import tofu.syntax.functionK
import tofu.syntax.functionK.funK
import tofu.syntax.monadic._

trait DayClosure[F[_], G[_], A] {
  def apply[B, C](fb: F[B])(f: (A, B) => Eval[C]): G[C]
}

object DayClosure {
  def apply[F[_], A] = new Make[F, A](true)
  def mk[F[_], A]    = new Make[F, A](true)

  def id[F[_]: Functor, A](a: A): DayClosure[F, F, A] =
    apply[F, A]((fx, f) => fx.map(f(a, _).value))

  class Make[F[_], A](private val __ : Boolean) extends AnyVal {
    type Arb1
    type Arb2
    def apply[G[_]](maker: Maker[F, G, A, Arb1, Arb2]): DayClosure[F, G, A] = maker
  }

  trait Maker[F[_], G[_], A, Ab, Ac] extends DayClosure[F, G, A] {
    def applyArb(fb: F[Ab], f: (A, Ab) => Eval[Ac]): G[Ac]

    override def apply[B, C](fb: F[B])(f: (A, B) => Eval[C]): G[C] =
      applyArb(fb.asInstanceOf[F[Ab]], f.asInstanceOf[(A, Ab) => Eval[Ac]]).asInstanceOf[G[C]]
  }

  def fromTrans[F[_], G[_]: Functor, A](fg: F ~> G)(a: A): DayClosure[F, G, A] =
    DayClosure[F, A]((fb, f) => fg(fb).map(b => f(a, b).value))

  def curry[F[_], G[_], H[_]](f: Day[F, G, *] ~> H): F ~> DayClosure[G, H, *] =
    funK(fa => DayClosure.mk((gb, g) => f(Day(fa, gb)(g))))

  def uncurry[F[_], G[_], H[_]](fk: F ~> DayClosure[G, H, *]): Day[F, G, *] ~> H =
    funK(fa => fk(fa.fx)(fa.gy)(fa.comb))

  private def functorInstance[F[_], G[_]]: Functor[DayClosure[F, G, *]] = new Functor[DayClosure[F, G, *]] {
    override def map[A, B](fga: DayClosure[F, G, A])(f: A => B): DayClosure[F, G, B] =
      DayClosure[F, B]((fx, g) => fga(fx)((a, x) => g(f(a), x)))
  }

  private val dayClosureFunctorAny = functorInstance[Any, Any]

  implicit def dayClosureFunctor[F[_], G[_]]: Functor[DayClosure[F, G, *]] =
    dayClosureFunctorAny.asInstanceOf[Functor[DayClosure[F, G, *]]]
}
