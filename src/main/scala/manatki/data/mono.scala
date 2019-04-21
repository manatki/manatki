package manatki.data

import cats.evidence.Is
import cats.{Eval, Foldable, Functor, Monad}

sealed trait MonoStr[A] {
  def is: Is[A, Char]
  def str: String
}

object MonoStr {
  def apply(s: String): MonoStr[Char] = Impl(s)

  final case class Impl(str: String) extends MonoStr[Char] {
    def is = Is.refl
  }

  implicit val monoStrFoldable: Foldable[MonoStr] = new Foldable[MonoStr] {
    def foldLeft[A, B](fa: MonoStr[A], b: B)(f: (B, A) => B): B = {
      val fs = fa.is.substitute[(B, ?) => B](f)
      fa.str.foldLeft(b)(fs)
    }

    def foldRight[A, B](fa: MonoStr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      val fs = fa.is.substitute[(?, Eval[B]) => Eval[B]](f)
      def go(i: Int): Eval[B] =
        if (i == fa.str.length) lb
        else Eval.defer(fs(fa.str(i), go(i + 1)))

      go(0)
    }
  }
}

trait MonoFunctor[A, B] {
  def map(a: A)(f: B => B): A
}

object MonoFunctor {
  implicit def fromFunctor[F[_], A](implicit F: Functor[F]): MonoFunctor[F[A], A] = new MonoFunctor[F[A], A] {
    def map(a: F[A])(f: A => A): F[A] = F.map(a)(f)
  }
}

trait MonoFoldable[A, B] {
  def foldLeft[C](a: A, c: C)(f: (C, B) => C): C

  def foldRight[C](a: A, lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C]
}

object MonoFoldable {
  implicit def fromFoldable[F[_], A](implicit F: Foldable[F]): MonoFoldable[F[A], A] = new MonoFoldable[F[A], A] {
    def foldLeft[C](a: F[A], c: C)(f: (C, A) => C): C = F.foldLeft(a, c)(f)

    def foldRight[C](a: F[A], lc: Eval[C])(f: (A, Eval[C]) => Eval[C]): Eval[C] = F.foldRight(a, lc)(f)
  }
}

trait MonoMonad[A, B] extends MonoFunctor[A, B] {
  def pure(b: B): A

  def flatMap(a: A)(f: B => A): A

  def map(a: A)(f: B => B): A = flatMap(a)(b => pure(f(b)))
}

object MonoMonad {
  implicit def fromMonad[F[_], A](implicit F: Monad[F]): MonoMonad[F[A], A] =
    new MonoMonad[F[A], A] {
      def pure(b: A): F[A] = F.pure(b)

      def flatMap(a: F[A])(f: A => F[A]): F[A] = F.flatMap(a)(f)
    }
}

trait MonoTraverse[A, B] {
  def traverse[C](a: A)(f: B => C)(implicit C: MonoMonad[C, B]): C
}
