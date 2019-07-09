package manatki.free

import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.{Monoid, Semigroup}

trait Capture[-T[_]] {
  def continue[A](k: T[A]): A
}
trait PointedE[-A, B]   extends (A => B) { def empty: B }
trait SemigroupE[-A, B] extends Semigroup[B] with (A => B)
trait MonoidE[-A, B]    extends Monoid[B] with SemigroupE[A, B] with PointedE[A, B]

object Lists {
  type FreeSemigroup[+A] = Capture[SemigroupE[A, ?]]
  type FreeMonoid[+A]    = Capture[MonoidE[A, ?]]
  type FreePointed[+A]   = Capture[PointedE[A, ?]]

  def single[A](a: A): Capture[A => ?] = new Capture[A => ?] {
    def continue[B](k: A => B): B = k(a)
  }

  def combine[A, F[x] <: SemigroupE[A, x]](xs: Capture[F], ys: Capture[F]): Capture[F] =
    new Capture[F] {
      def continue[B](k: F[B]): B = k.combine(xs.continue(k), ys.continue(k))
    }

  def empty[A, F[x] <: PointedE[A, x]]: Capture[F] =
    new Capture[F] {
      def continue[B](k: F[B]): B = k.empty
    }

  def fromOption[A](opt: Option[A]): FreePointed[A] =
    new Capture[PointedE[A, ?]] {
      def continue[B](k: PointedE[A, B]): B = opt.fold(k.empty)(k.apply)
    }

  def fromNel[A](nel: NonEmptyList[A]): FreeSemigroup[A] =
    new Capture[SemigroupE[A, ?]] {
      def continue[B](k: SemigroupE[A, B]): B = nel.reduceMap(k.apply)(k)
    }

  def fromList[A](l: List[A]): FreeMonoid[A] =
    new Capture[MonoidE[A, ?]] {
      def continue[B](k: MonoidE[A, B]): B = l.foldMap(k.apply)(k)
    }

  def toNel[A](nel: FreeSemigroup[A]): NonEmptyList[A] =
    nel.continue(new SemigroupE[A, NonEmptyList[A]] {
      def apply(a: A): NonEmptyList[A]                                     = NonEmptyList.one(a)
      def combine(x: NonEmptyList[A], y: NonEmptyList[A]): NonEmptyList[A] = x ::: y
    })

  def toOption[A](opt: FreePointed[A]): Option[A] =
    opt.continue(new PointedE[A, Option[A]] {
      def empty: Option[A]       = None
      def apply(a: A): Option[A] = Some(a)
    })

  def toList[A](l: FreeMonoid[A]): List[A] =
    l.continue(new MonoidE[A, List[A]] {
      def apply(a: A): List[A]                     = List(a)
      def empty: List[A]                           = Nil
      def combine(x: List[A], y: List[A]): List[A] = x ::: y
    })

  def checkSM[A, B <: A] = implicitly[FreeSemigroup[B] <:< FreeMonoid[A]]
  def checkOM[A, B <: A] = implicitly[FreePointed[B] <:< FreeMonoid[A]]
}




