package manatki.free
import alleycats.Pure
import cats.free.Free
import cats.{Applicative, Functor, Monad, StackSafeMonad, ~>}
import manatki.control.Selective
import manatki.syntax.functionK

trait Capture1[-K[_[_]], A] {
  def continue[F[_]](k: K[F]): F[A]
}

trait PureE[-F[_], G[_]]        extends FunK[F, G] with Pure[G]
trait FunctorE[-F[_], G[_]]     extends FunK[F, G] with Functor[G]
trait ApplicativeE[-F[_], G[_]] extends FunctorE[F, G] with PureE[F, G] with Applicative[G]
trait SelectiveE[-F[_], G[_]]   extends ApplicativeE[F, G] with Selective[G]

trait MonadE[-F[_], G[_]] extends Monad[G] with SelectiveE[F, G] {
  def select[A, B](fab: G[Either[A, B]], fa: => G[B]): G[B] =
    flatMap(fab) {
      case Left(a)  => fa
      case Right(b) => pure(b)
    }
}

object Functors {
  type FreeF[+F[_], A]           = Capture1[FunK[F, ?[_]], A]
  type FreeFunctor[+F[_], A]     = Capture1[FunctorE[F, ?[_]], A]
  type FreePure[+F[_], A]        = Capture1[PureE[F, ?[_]], A]
  type FreeApplicative[+F[_], A] = Capture1[ApplicativeE[F, ?[_]], A]
  type FreeSelective[+F[_], A]   = Capture1[SelectiveE[F, ?[_]], A]
  type FreeMonad[+F[_], A]       = Capture1[MonadE[F, ?[_]], A]

  def embed[F[_], A](fa: F[A]): FreeF[F, A] =
    new FreeF[F, A] {
      def continue[G[_]](k: FunK[F, G]): G[A] = k(fa)
    }

  trait PureInstance[F[_], K[g[_]] <: PureE[F, g]] extends Pure[Capture1[K, ?]] {
    override def pure[A](x: A) = new Capture1[K, A] {
      def continue[G[_]](k: K[G]): G[A] = k.pure(x)
    }
  }

  trait FunctorInstance[F[_], K[g[_]] <: FunctorE[F, g]] extends Functor[Capture1[K, ?]] {
    override def map[A, B](fa: Capture1[K, A])(f: A => B) =
      new Capture1[K, B] {
        def continue[G[_]](k: K[G]): G[B] = k.map(fa.continue(k))(f)
      }
  }

  trait ApplicativeInstance[F[_], K[g[_]] <: ApplicativeE[F, g]]
      extends Applicative[Capture1[K, ?]] with FunctorInstance[F, K] with PureInstance[F, K] {

    override def ap[A, B](ff: Capture1[K, A => B])(fa: Capture1[K, A]) =
      new Capture1[K, B] {
        def continue[G[_]](k: K[G]): G[B] = k.ap(ff.continue(k))(fa.continue(k))
      }
  }

  trait SelectiveInstance[F[_], K[g[_]] <: SelectiveE[F, g]]
      extends Selective[Capture1[K, ?]] with ApplicativeInstance[F, K] {
    def select[A, B](fab: Capture1[K, Either[A, B]], fa: => Capture1[K, B]) =
      new Capture1[K, B] {
        def continue[G[_]](k: K[G]): G[B] = k.select(fab.continue(k), fa.continue(k))
      }
  }

  trait MonadInstance[F[_], K[g[_]] <: MonadE[F, g]]
      extends StackSafeMonad[Capture1[K, ?]] with SelectiveInstance[F, K] {
    def flatMap[A, B](fa: Capture1[K, A])(f: A => Capture1[K, B]) =
      new Capture1[K, B] {
        def continue[G[_]](k: K[G]): G[B] = k.flatMap(fa.continue(k))(a => f(a).continue(k))
      }
  }
//
  implicit def functor[F[_]]: Functor[FreeFunctor[F, ?]] = new FunctorInstance[F, FunctorE[F, ?[_]]] {}
  implicit def pure[F[_]]: Pure[FreePure[F, ?]]          = new PureInstance[F, PureE[F, ?[_]]]       {}
  implicit def applicative[F[_]]: Applicative[FreeApplicative[F, ?]] =
    new ApplicativeInstance[F, ApplicativeE[F, ?[_]]] {}
  implicit def selective[F[_]]: Selective[FreeSelective[F, ?]] = new SelectiveInstance[F, SelectiveE[F, ?[_]]] {}
  implicit def monad[F[_]]: Monad[FreeMonad[F, ?]]             = new MonadInstance[F, MonadE[F, ?[_]]]         {}

  def fromFreeMonad[F[_], A](fa: cats.free.Free[F, A]): FreeMonad[F, A] =
    fa.foldMap[FreeMonad[F, ?]](functionK[F](fa => embed(fa)))

  def toFreeMonad[F[_], X](fa: FreeMonad[F, X]): Free[F, X] =
    fa.continue(new MonadE[F, Free[F, ?]] with StackSafeMonad[Free[F, ?]] {
      def pure[A](a: A): Free[F, A]                                       = Free.pure(a)
      def apply[A](fa: F[A]): Free[F, A]                                  = Free.liftF(fa)
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B]   = fa.flatMap(f)
    })

}
