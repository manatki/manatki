package manatki.data.tagless
import cats.free.Free
import cats.{Eval, Functor, Id, ~>}
import manatki.data.day.{Day, DayClosure, FunctionD2}
import simulacrum.typeclass
import tofu.syntax.monadic._

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
  override def hpoint[A](a: A): U[Id, A] = hpure[Id, A](a)
}

@typeclass trait DMonoidal[U[_[_], _]] extends DSemigroupal[U] with HPoint[U] {
  override def hmap[F[_]: Functor, G[_]: Functor, A](ufa: U[F, A])(fk: F ~> G): U[G, A] =
    dmap2(ufa, hpoint[Unit](()))((a, _) => Eval.now(a))(FunctionD2[F, Id]((fa, b, f) => fk(fa).map(f(_, b).value)))
}

@typeclass trait DFlatMap[U[_[_], _]] extends DSemigroupal[U] {
  def dflatten[F[_]: Functor, A](uuf: U[U[F, *], A]): U[F, A] =
    dflatMap(uuf, DayClosure.id[U[F, *], Unit](()))((a, _) => Eval.now(a))

  def dflatMap[F[_]: Functor, G[_]: Functor, A, B, C](fa: U[F, A], k: DayClosure[F, U[G, *], B])(
      f: (A, B) => Eval[C]
  ): U[G, C]
}

@typeclass trait DMonad[U[_[_], _]] extends DFlatMap[U] with DMonoidal[U] with HPure[U] {
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

object DMonad {
  implicit val freeMonadInstance: DMonad[Free] = new DMonad[Free] {
    def hpure[F[_]: Functor, A](fa: F[A]): Free[F, A] = Free.liftF(fa)
    def dflatMap[F[_]: Functor, G[_]: Functor, A, B, C](fa: Free[F, A], k: DayClosure[F, Free[G, *], B])(
        f: (A, B) => Eval[C]
    ): Free[G, C] =
      fa.fold[Free[G, C]](
        a => f(a, ??? : B).value.pure[Free[G, *]],
        ff => k(ff)((b: B, fr) => Eval.now(dflatMap(fr, k)(f))).flatten
      )
    implicit def functor[F[_]: Functor]: Functor[Free[F, *]] = implicitly
  }
}
