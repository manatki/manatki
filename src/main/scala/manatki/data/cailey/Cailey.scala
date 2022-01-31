package manatki.data.cailey

import cats.{Applicative, Functor}
import cats.syntax.all._

/** Is this the proper Scala encoding for the Haskell snippet in comments ? */
/** from : https://doisinkidney.com/pdfs/algebras-for-weighted-search.pdf */

// newtype Cayley f a = Cayley {runC :: ∀b. f b → f (a, b) }
trait Cayley[F[_], A] {
  def apply[B](fb: F[B]): F[(A, B)]

// rep :: Applicative f ⇒ Cayley f a → f a
// rep x = fmap fst (runC x (pure ()))
  def rep(implicit F: Applicative[F]): F[A] = apply(F.unit).map(_._1)
}

object Cailey {
  // abs :: Applicative f ⇒ f a → Cayley f a
  // abs x = Cayley (liftA2 (, ) x)
  def apply[F[_]: Applicative, A](fa: F[A]): Cayley[F, A] =
    new Cayley[F, A] {
      def apply[B](fb: F[B]): F[(A, B)] = (fa, fb).tupled
    }

// instance Functor f ⇒ Applicative (Cayley f ) where
// pure x = Cayley (fmap (x, ))
// fs <*> xs = Cayley (fmap (𝜆(f , (x, xs)) → (f x, xs)) ◦ runC fs ◦ runC xs)
  implicit def applicative[F[_]: Functor] = new Applicative[Cayley[F, *]] {
    def pure[A](a: A) = new Cayley[F, A] {
      def apply[B](fb: F[B]): F[(A, B)] = fb.tupleLeft(a)
    }

    def ap[A, B](fs: Cayley[F, A => B])(xs: Cayley[F, A]) =
      new Cayley[F, B] {
        def apply[C](fb: F[C]): F[(B, C)] = fs(xs(fb)).map { case (f, (x, xs)) => (f(x), xs) }
      }
  }
}
