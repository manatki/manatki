package manatki.syntax

import cats._
import cats.data.OptionT
import cats.free.Free
import cats.syntax.foldable._

object foldable {
  implicit class FoldableOps[F[_], A](val xs: F[A]) extends AnyVal {
    // by @odomontois requested by @tvaroh 10.01.2018 13:22
    /**collect first item returning Some(_) by monadic action, stack-safety relies on underlying flatMap*/
    def collectFirstM[B, M[_]](f: A => M[Option[B]])(implicit M: Monad[M], F: Foldable[F]): M[Option[B]] =
      xs.foldRight(Now(OptionT.none[M, B]))(
        (x, opt) => Later(OptionT(f(x)).orElse(opt.value))
      ).value.value


    /**collect first item returning Some(_) by monadic action, stack safe*/
    def collectFirstSM[B, M[_]](f: A => M[Option[B]])(implicit M: Monad[M], F: Foldable[F]): M[Option[B]] =
      xs.foldRight(Now(OptionT.none[Free[M, ?], B]))(
        (x, opt) => Later(OptionT(Free.liftF(f(x))).orElse(opt.value))
      ).value.value.runTailRec
  }
}
