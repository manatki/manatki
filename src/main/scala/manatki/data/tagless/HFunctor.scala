package manatki.data.tagless
import cats.arrow.FunctionK
import cats.free.{Cofree, Free}
import cats.{Eval, Functor, Id, ~>}
import manatki.data.day.{Day, DayClosure, FunctionD2}
import simulacrum.typeclass
import tofu.syntax.functionK
import tofu.syntax.functionK.funK
import tofu.syntax.monadic._
import cats.Monad
import cats.StackSafeMonad
import manatki.data.tagless.HFree.Pure
import manatki.free.FunK
import manatki.data.tagless.HFree.Bind
import manatki.data.tagless.HFree.HBind

@typeclass
trait HFunctor[U[_[_], _]] {
  implicit def functor[F[_]: Functor]: Functor[U[F, *]]
  def hmap[F[_]: Functor, G[_]: Functor, A](ufa: U[F, A])(fk: F ~> G): U[G, A]
}

@typeclass
trait DSemigroupal[U[_[_], _]] extends HFunctor[U] {
  def dmap2[F[_]: Functor, G[_]: Functor, H[_]: Functor, A, X, Y](fx: U[F, X], gy: U[G, Y])(xya: (X, Y) => Eval[A])(
      f: FunctionD2[F, G, H]
  ): U[H, A]

  def dproduct[F[_]: Functor, G[_]: Functor, A](d: Day[U[F, *], U[G, *], A]): U[Day[F, G, *], A] =
    dmap2(d.fx, d.gy)(d.comb)(FunctionD2[F, G]((fx, gy, f) => Day(fx, gy)(f)))

  def dmap2d[F[_]: Functor, G[_]: Functor, H[_]: Functor, A](
      fga: Day[U[F, *], U[G, *], A]
  )(f: FunctionD2[F, G, H]): U[H, A] =
    dmap2(fga.fx, fga.gy)(fga.comb)(f)

  def hap[F[_]: Functor, G[_]: Functor, A, B](hf: U[DayClosure[F, G, *], A => B])(uf: U[F, A]): U[G, B] =
    dmap2(hf, uf)((f, a) => Eval.later(f(a)))(FunctionD2[DayClosure[F, G, *], F]((fgx, fx, f) => fgx(fx)(f)))
}

@typeclass trait HPoint[U[_[_], _]] {
  def hpoint[A](a: A): U[Id, A]
}

@typeclass trait HPure[U[_[_], _]] extends HPoint[U] {
  def hpure[F[_]: Functor, A](fa: F[A]): U[F, A]

  def hpuref[F[_]: Functor, A]: F ~> U[F, *] = funK(hpure(_))

  override def hpoint[A](a: A): U[Id, A] = hpure[Id, A](a)
}

@typeclass trait DMonoidal[U[_[_], _]] extends DSemigroupal[U] with HPoint[U] {
  override def hmap[F[_]: Functor, G[_]: Functor, A](ufa: U[F, A])(fk: F ~> G): U[G, A] =
    dmap2(ufa, hpoint[Unit](()))((a, _) => Eval.now(a))(FunctionD2[F, Id]((fa, b, f) => fk(fa).map(f(_, b).value)))
}

@typeclass trait HFlatMap[U[_[_], _]] extends HFunctor[U] {
  def hflatten[F[_]: Functor, A](uuf: U[U[F, *], A]): U[F, A] = hflatMap(uuf)(FunctionK.id)

  def hflatMap[F[_]: Functor, G[_]: Functor, A](tfa: U[F, A])(t: F ~> U[G, *]): U[G, A]
}

@typeclass trait HMonad[U[_[_], _]] extends HFlatMap[U] with HPure[U] {
  override def hmap[F[_]: Functor, G[_]: Functor, A](ufa: U[F, A])(fk: F ~> G): U[G, A] =
    hflatMap(ufa)(funK(fx => hpure(fk(fx))))
}

object HMonad {
  implicit val freeMonadInstance: HMonad[Free] = new HMonad[Free] {
    def hpure[F[_]: Functor, A](fa: F[A]): Free[F, A] = Free.liftF(fa)

    implicit def functor[F[_]: Functor]: Functor[Free[F, *]] = implicitly

    def hflatMap[F[_]: Functor, G[_]: Functor, A](tfa: Free[F, A])(t: F ~> Free[G, *]): Free[G, A] = tfa.foldMap(t)
  }
}
@typeclass trait DFlatMap[U[_[_], _]] extends HFlatMap[U] {
  override def hflatMap[F[_]: Functor, G[_]: Functor, A](tfa: U[F, A])(t: F ~> U[G, *]): U[G, A] =
    dflatMap(tfa, DayClosure.fromTrans(t)(()))((a, _) => Eval.now(a))

  def dflatMap[F[_]: Functor, G[_]: Functor, A, B, C](fa: U[F, A], k: DayClosure[F, U[G, *], B])(
      f: (A, B) => Eval[C]
  ): U[G, C]
}

// Tensorial (day) strong monad
@typeclass trait DMonad[U[_[_], _]] extends DFlatMap[U] with DMonoidal[U] with HMonad[U] {
  def strength[F[_]: Functor, G[_]: Functor, A](d: Day[F, U[G, *], A]): U[Day[F, G, *], A] =
    dflatMap[G, Day[F, G, *], d.Y, d.X, A](d.gy, DayClosure.mk((gy, f) => hpure(Day(d.fx, gy)(f))))(
      (y, x) => d.run(x, y)
    )

  override def dmap2[F[_]: Functor, G[_]: Functor, H[_]: Functor, A, X, Y](ufx: U[F, X], ugy: U[G, Y])(
      xya: (X, Y) => Eval[A]
  )(fgh: FunctionD2[F, G, H]): U[H, A] =
    dflatMap[F, H, X, Y, A](
      ufx,
      new DayClosure[F, U[H, *], Y] {
        def apply[D, E](fd: F[D])(yde: (Y, D) => Eval[E]): U[H, E] =
          dflatMap[G, H, Y, D, E](ugy, new DayClosure[G, U[H, *], D] {
            def apply[B, C](gb: G[B])(f: (D, B) => Eval[C]): U[H, C] =
              hpure(fgh(fd, gb)(f))
          })(yde)

      }
    )(xya)
}

sealed trait HFree[+U[+_[_], _], +F[_], A]

object HFree {
  final case class Pure[A](a: A) extends HFree[Nothing, Nothing, A]
  final case class Bind[+U[+_[_], _], +F[_], A, B](fa: F[A], cont: A => HFree[U, F, B]) extends HFree[U, F, B]
  final case class HBind[U[_[_], _], F[_], G[_], A, B](
      base: U[F, A],
      hcont: F FunK HFree[U, G, *],
      cont: A => HFree[U, G, B],
  ) extends HFree[U, G, B]

  val absurdity: Nothing FunK HFree[Nothing, Nothing, *] = FunK[Nothing](x => x)

  private [this] val monadInstanceAny = new MonadInstance[Any, Any]
  implicit def monadInstance[U[+_[_], _], F[_]] : Monad[HFree[U, F, *]] = 
    monadInstanceAny.asInstanceOf[Monad[HFree[U, F, *]]]

  class MonadInstance[U[+_[_], _], F[_]] extends StackSafeMonad[HFree[U, F, *]] {
    def flatMap[A, B](fa: HFree[U, F, A])(f: A => HFree[U, F, B]): HFree[U, F, B] = fa match {
      case Pure(a) => f(a)
      case fm: Bind[U, F, a, _] => Bind[U, F, a, B](fm.fa, a => flatMap(fm.cont(a))(f)) 
      case hfm: HBind[U, f, F, a, _] => 
        HBind[U, f, F, a, B](hfm.base, hfm.hcont, a => flatMap(hfm.cont(a))(f))
    }
    def pure[A](x: A): HFree[U, F, A] = Pure(x)
  }

  class HMonadInstace[U[+_[_], _]] extends HMonad[HFree[U, *[_], *]]{
    def functor[F[_]: Functor]: Functor[HFree[U, F, *]] = monadInstance
    def hflatMap[F[_]: Functor, G[_]: Functor, A](tfa: HFree[U,F,A])(t: F ~> HFree[U,G, *]): HFree[U,G,A] = 
      tfa match {
        case pa: Pure[A] => pa
        // case Bind(fa, cont) => hflatMap(t(fa))
        case HBind(base, hcont, cont) => ???
      }
    def hpure[F[_]: Functor, A](fa: F[A]): HFree[U,F,A] = Bind[Nothing,F, A, A](fa, Pure(_))
  }
}

object DMonad {
  trait ViaStrength[U[_[_], _]] extends DMonad[U] {
    def strength1[F[_]: Functor, G[_]: Functor, A](d: Day[F, U[G, *], A])
    def hflatMap1[F[_]: Functor, G[_]: Functor, A](tfa: Free[F, A])(t: F ~> Free[G, *]): Free[G, A]


    def dflatMap[F[_] : Functor, G[_] : Functor, A, B, C](fa: U[F, A], k: DayClosure[F, U[G, *], B])(f: (A, B) => Eval[C]): U[G, C] = ???
  }
}
