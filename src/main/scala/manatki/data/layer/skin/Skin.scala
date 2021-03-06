package manatki.data.layer.skin

import cats.Functor
import cats.arrow.Profunctor
import cats.syntax.profunctor._
import manatki.data.layer.skin

trait Skin[P[_, _], A] {
  def cont[B](pa: P[A, B]): B
}

object Skin {
  trait Fix[P[_, _]] extends Skin[P, Fix[P]]

  def fix[P[_, _]](l: Skin[P, Fix[P]]): Fix[P] =
    new Fix[P] {
      def cont[B](pa: P[Fix[P], B]): B = l.cont(pa)
    }

  def functor[P[_, _]: Profunctor]: Functor[Skin[P, *]] =
    new Functor[Skin[P, *]] {
      def map[A, C](fa: Skin[P, A])(f: A => C): Skin[P, C] =
        new Skin[P, C] {
          def cont[B](pa: P[C, B]): B = fa.cont(pa.lmap(f))
        }
    }

  implicit def construct[P[_, _]](implicit P: Skinny[P]): P[Skin.Fix[P], Skin.Fix[P]] = P.construct

  implicit def functorInstance[P[_, _]: Profunctor]: Functor[Skin[P, *]] = functor(Profunctor[P])
}

trait Coalgebra[P[_, _], A] {
  def cont[B](a: A, pa: P[A, B]): B
}

trait Recursive[X] {
  type P[_, _]
  implicit def profunctor: Profunctor[P]
  def coalgebra: Coalgebra[P, X]
  def cata[B](x: X, alg: Algebra[P, B]): B = Hylo(alg, coalgebra).apply(x)
}

trait Corecursive[X] {
  type P[_, _]
  implicit def profunctor: Profunctor[P]
  def algebra: Algebra[P, X]
  def ana[B](b: B, coalg: Coalgebra[P, B]): X = skin.Hylo(algebra, coalg).apply(b)
}

trait Mu[P[_, _]] {
  def alg: Algebra[P, Mu[P]]

  def cata[A](alg: Algebra[P, A]): A

  def peel[B](p: P[Mu[P], B]): B
}

object Mu {
  implicit def murecursive[P1[_, _]: Profunctor]: Recursive[Mu[P1]] = new Recursive[Mu[P1]] {
    type P[x, y] = P1[x, y]
    val profunctor: Profunctor[P1] = Profunctor[P1]
    val coalgebra = new Coalgebra[P, Mu[P]] {
      def cont[B](a: Mu[P], pa: P[Mu[P], B]): B = a.peel(pa)
    }

  }
}

trait Birecursive[X] extends Recursive[X] with Corecursive[X]

final case class Hylo[P[_, _]: Profunctor, A, B](alg: Algebra[P, B], coalg: Coalgebra[P, A]) extends (A => B) {
  def apply(a: A): B = coalg.cont(a, alg.lmap(this))
}
