package manatki.data.tagless
import cats.{Applicative, Functor, Id, Monad}
import manatki.data.tagless.Rep.prof
import simulacrum.typeclass

import scala.annotation.tailrec
import scala.annotation.unchecked.{uncheckedVariance => uv}

trait Rep[-F[_]] extends Layer[λ[(`-a`, `b`) => F[b]]] {
  def apply[R](fa: F[R]): R

  override def unpack[R](p: F[R]): R = apply(p)
}

@typeclass
trait LMap[P[_, _]] {
  def lmap[A, B, C](fab: P[A, B])(f: C => A): P[C, B]
}

@typeclass trait RMap[P[_, _]] {
  def rmap[A, B, C](fab: P[A, B])(f: B => C): P[A, C]

}

@typeclass
trait Pro[P[_, _]] extends LMap[P] with RMap[P] { self =>
  def dimap[A, B, C, D](fab: P[A, B])(f: C => A)(g: B => D): P[C, D] = rmap(lmap(fab)(f))(g)
}

object Rep {
  type prof[-P[a, b], A] = Rep[P[A, *]]

  def apply[U[_]]     = new Applied[U](true)
  def mk[U[_]]        = new Applied[U](true)
  def pro[P[_, _], A] = new Applied[P[A, *]](true)

  class Applied[T[_]](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(maker: MakeRepr[T, Arb]): Rep[T] = maker
  }

  abstract class MakeRepr[T[_], Arb] extends Rep[T] {
    def applyArbitrary(fk: T[Arb]): Arb

    def apply[R](fk: T[R]): R = applyArbitrary(fk.asInstanceOf[T[Arb]]).asInstanceOf[R]
  }

  implicit def prorepFunctor[P[_, _]](implicit P: Pro[P]): Functor[prof[P, *]] = new Functor[prof[P, *]] {
    def map[A, B](fa: prof[P, A])(f: A => B): prof[P, B] = Rep.pro[P, B](p => fa(P.lmap(p)(f)))
  }

  implicit class ProfRepOps[P[-_, _], A](private val self: Rep[P[A, *]]) extends AnyVal {
    def pmap[B](f: A => B)(implicit P: Pro[P]): Rep[P[B, *]] = Rep.pro[P, B](p => self(P.lmap(p)(f)))
  }
}

trait Representable[F[_]] extends Monad[F] with ProCorep[λ[(a, b) => F[b]]] {
  def tabulate[A](fa: Rep[F] => A): F[A]

  override def cotabulate[A, B](k: Rep[F] => B): F[B]    = tabulate(k)
  override def lmap[A, B, C](fab: F[B])(f: C => A): F[B] = fab

  override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = tabulate(r => r(f(r(fa))))
  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = {
    @tailrec def go(a: A)(r: Rep[F]): B = r(f(a)) match {
      case Left(value)  => go(value)(r)
      case Right(value) => value
    }
    tabulate(go(a))
  }
  override def pure[A](x: A): F[A]                         = tabulate(_ => x)
}

object Representable {
  def index[F[_], A](fa: F[A], r: Rep[F]): A = r.apply(fa)
}

@typeclass
trait ProCorep[P[_, _]] extends Pro[P] {
  type PR[A] = Rep[P[A, *]]

  def cotabulate[A, B](k: Rep[P[A, *]] => B): P[A, B]

  override def rmap[A, B, C](fab: P[A, B])(f: B => C): P[A, C] = cotabulate(rep => f(rep(fab)))

  def zip[A, B, C, D](pab: P[A, B], pcd: P[C, D]): P[(A, C), (B, D)] =
    cotabulate(rep => (rep(lmap(pab)(_._1)), rep(lmap(pcd)(_._2))))

  def merge[A, B, C](pab: P[A, B], pac: P[A, C]): P[A, (B, C)] =
    cotabulate(rep => (rep(pab), rep(pac)))

  def functor: Functor[Rep.prof[P, *]] = new Functor[prof[P, *]] {
    def map[A, B](fa: prof[P, A])(f: A => B): prof[P, B] = fa(lmap(cotabulate(identity[Rep[P[B, *]]]))(f))
  }

  def constant[A, B](b: B): P[A, B] = cotabulate(_ => b)

  def construct[Q[-x, y] <: P[x, y] @uv]: P[Layer[Q], Layer[Q]] = cotabulate(f => Layer[Q](f(_)))

}

object ProCorep {
  def construct[P[-_, _]](implicit P: ProCorep[P]): P[Layer[P], Layer[P]] = P.construct
}

@typeclass
trait ProTraverse[P[_, _]] extends ProCorep[P] {
  def prosequence[F[_], A, B](p: P[A, B])(implicit F: Applicative[F]): P[F[A], F[B]] =
    tabTraverse[F, F[A], A, F[B]](identity)(F.map(_)(_(p)))

  override def cotabulate[A, B](k: PR[A] => B): P[A, B] =
    tabTraverse[Id, A, A, B](identity)(k)

  override def lmap[A, B, C](fab: P[A, B])(f: C => A): P[C, B] =
    tabTraverse[Id, C, A, B](f)(_(fab))

  def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): P[A, C]
}

object ProTraverse {

  class Tab[F[_], A, B, C, P[-_, _]](val left: A => F[B], val right: F[Rep[P[B, *]]] => C)(implicit
      val F: Applicative[F]
  ) {
    final def rep = Rep.pro[P, B]
  }

  def make[P[-_, _]] = new Maker[P]

  class Maker[P[-_, _]] {
    type Fa[_]
    type Aa
    type Ba
    type Ca
    def apply(instance: Applied[Fa, Aa, Ba, Ca, P]): ProTraverse[P] = instance
  }

  abstract class Applied[Fa[_], Aa, Ba, Ca, P[-_, _]] extends ProTraverse[P] {
    def tabTravArb(left: Aa => Fa[Ba])(right: Fa[Rep[P[Ba, *]]] => Ca)(F: Applicative[Fa]): P[Aa, Ca]

    override def tabTraverse[F[_], A, B, C](left: A => F[B])(right: F[Rep[P[B, *]]] => C)(implicit
        F: Applicative[F]
    ): P[A, C] =
      tabTravArb(left.asInstanceOf[Aa => Fa[Ba]])(right.asInstanceOf[Fa[Rep[P[Ba, *]]] => Ca])(
        F.asInstanceOf[Applicative[Fa]]
      ).asInstanceOf[P[A, C]]
  }
}
