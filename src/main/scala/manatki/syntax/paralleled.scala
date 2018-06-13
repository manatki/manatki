package manatki.syntax

import cats.{Foldable, Traverse}
import manatki.Paralleled
import manatki.Splitting

object splitting extends Splitting.ToSplittingOps {
  implicit class ParallelSequenceOps[T[_], F[_], A](val ta: T[F[A]]) extends AnyVal {
    def parSequence(implicit T: Traverse[T], P: Paralleled[F]): F[T[A]] =
      P.sequential(T.traverse[P.Par, F[A], A](ta)(a => P.parallel(a))(P.applicative))

    def parSequence_(implicit T: Foldable[T], P: Paralleled[F]): F[Unit] =
      P.sequential(T.traverse_[P.Par, F[A], A](ta)(a => P.parallel(a))(P.applicative))
  }
}

object paralleled extends Paralleled.ToParalleledOps {
  implicit class ParallelSequenceOps[T[_], F[_], A](val ta: T[F[A]]) extends AnyVal {
    def parSequence(implicit T: Traverse[T], P: Paralleled[F]): F[T[A]] =
      P.sequential(T.traverse[P.Par, F[A], A](ta)(a => P.parallel(a))(P.applicative))

    def parSequence_(implicit T: Foldable[T], P: Paralleled[F]): F[Unit] =
      P.sequential(T.traverse_[P.Par, F[A], A](ta)(a => P.parallel(a))(P.applicative))
  }

  implicit class ParallelTraverseOps[T[_], A](val ta: T[A]) extends AnyVal {
    def parTraverse[F[_], B](f: A => F[B])(implicit T: Traverse[T], P: Paralleled[F]): F[T[B]] =
      P.sequential(T.traverse[P.Par, A, B](ta)(a => P.parallel(f(a)))(P.applicative))

    def parTraverse_[F[_], B](f: A => F[B])(implicit T: Foldable[T], P: Paralleled[F]): F[Unit] =
      P.sequential(T.traverse_[P.Par, A, B](ta)(a => P.parallel(f(a)))(P.applicative))
  }
}
