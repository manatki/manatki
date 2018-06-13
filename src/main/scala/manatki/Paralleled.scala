package manatki

import cats.{Applicative, Apply, FlatMap, Monad, NonEmptyParallel, Parallel}
import simulacrum.{op, typeclass}

//discussed with @vpavkin and @catostrophe 11.06.2016 8:55-12:34
@typeclass
trait Paralleled[F[_]] extends Splitting[F] {
  override type Par[a]
  override def parallel[a](fa: F[a]): Par[a]
  override def sequential[a](pa: Par[a]): F[a]
  override def seqInstance: Monad[F]
  override def parInstance: Applicative[Par]

  def monad: Monad[F] = seqInstance
  def applicative: Applicative[Par] = parInstance
}
@typeclass
trait Splitting[F[_]] {
  type Par[a]
  def parallel[a](fa: F[a]): Par[a]
  def sequential[a](pa: Par[a]): F[a]
  def seqInstance: FlatMap[F]
  def parInstance: Apply[Par]

  def flatMap: FlatMap[F] = seqInstance
  def apply: Apply[Par] = parInstance

  def parMap2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
    sequential(parInstance.map2(parallel(fa), parallel(fb))(f))

  def parMap3[A, B, C, D](fa: F[A])(fb: F[B], fc: F[C])(
    f: (A, B, C) => D): F[D] =
    sequential(parInstance.map3(parallel(fa), parallel(fb), parallel(fc))(f))

  def parMap4[A, B, C, D, E](fa: F[A])(fb: F[B], fc: F[C], fd: F[D])(
    f: (A, B, C, D) => E): F[E] =
    sequential(
      parInstance.map4(parallel(fa), parallel(fb), parallel(fc), parallel(fd))(
        f))

  @op("&>", alias = true)
  def parProductR[A, B](fa: F[A])(fb: F[B]): F[B] =
    sequential(parInstance.productR(parallel(fa))(parallel(fb)))

  @op("<&", alias = true)
  def parProductL[A, B](fa: F[A])(fb: F[B]): F[A] =
    sequential(parInstance.productL(parallel(fa))(parallel(fb)))

  def parProduct[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    sequential(parInstance.product(parallel(fa), parallel(fb)))

  @op("<&>", alias = true)
  def parAp[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    sequential(parInstance.ap(parallel(fab))(parallel(fa)))
}

object Paralleled {
  type Aux[F[_], G[_]] = Paralleled[F] {type Par[a] = G[a]}

  implicit def fromCats[F[_], G[_]](implicit p: Parallel[F, G]): Aux[F, G] =
    new Paralleled[F] {
      override type Par[a] = G[a]
      override def parallel[a](fa: F[a]): G[a] = p.parallel(fa)
      override def sequential[a](pa: G[a]): F[a] = p.sequential(pa)
      override def seqInstance: Monad[F] = p.monad
      override def parInstance: Applicative[G] = p.applicative
    }
}

object Splitting {
  type Aux[F[_], G[_]] = Splitting[F] {type Par[a] = G[a]}

  implicit def fromCats[F[_], G[_]](
    implicit p                              : NonEmptyParallel[F, G]): Aux[F, G] = new Splitting[F] {
    override type Par[a] = G[a]
    override def parallel[a](fa: F[a]): G[a] = p.parallel(fa)
    override def sequential[a](pa: G[a]): F[a] = p.sequential(pa)
    override def seqInstance: FlatMap[F] = p.flatMap
    override def parInstance: Apply[G] = p.apply
  }
}
