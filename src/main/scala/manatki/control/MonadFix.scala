package manatki.control
import cats.data._
import cats.kernel.Monoid
import cats.{Eval, Monad}
import manatki.control.MonadFix.MonadFixImpl

trait MonadFix[F[_]] {
  implicit def monad: Monad[F]

  def mfix[A](fa: Eval[A] => F[A]): F[A]
}

object MonadFix extends MonadFixInstances {
  implicit val evalMfix: MonadFix[Eval] = new MonadFixImpl[Eval] {
    def mfix[A](fa: Eval[A] => Eval[A]): Eval[A] = {
      lazy val res: Eval[A] = Eval.defer(fa(res))
      res
    }
  }

  abstract class MonadFixImpl[F[_]](implicit val monad: Monad[F]) extends MonadFix[F]
}

trait MonadFixInstances {
  implicit def readerTMFix[F[_], R](implicit MF: MonadFix[F]): MonadFix[ReaderT[F, R, *]] = {
    import MF.monad
    new MonadFixImpl[ReaderT[F, R, *]] {
      def mfix[A](fa: Eval[A] => ReaderT[F, R, A]): ReaderT[F, R, A] =
        ReaderT(r => MF.mfix(fa(_).run(r)))
    }
  }

  implicit def writerTMFix[F[_], W: Monoid](implicit MF: MonadFix[F]): MonadFix[WriterT[F, W, *]] = {
    import MF.monad
    new MonadFixImpl[WriterT[F, W, *]] {
      def mfix[A](fa: Eval[A] => WriterT[F, W, A]): WriterT[F, W, A] =
        WriterT(MF.mfix(ewa => fa(ewa.map(_._2)).run))
    }
  }

  implicit def stateTMFix[F[_], S: Monoid](implicit MF: MonadFix[F]): MonadFix[StateT[F, S, *]] = {
    implicit def fmonad: Monad[F] = MF.monad
    new MonadFixImpl[StateT[F, S, *]] {
      def mfix[A](fa: Eval[A] => StateT[F, S, A]): StateT[F, S, A] = {
        StateT(s => MF.mfix(esa => fa(esa.map(_._2)).run(s)))
      }
    }
  }
}
