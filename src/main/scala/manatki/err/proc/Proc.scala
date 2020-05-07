package manatki.err.proc

import cats.{Bifunctor, Monad, StackSafeMonad}

import scala.annotation.tailrec

trait BiMonad[L[_, _], R[_, _]] {
  def left[A, B](a: A): L[A, B]
  def right[A, B](b: B): R[A, B]

  def leftFlatMap[A, B, C, D](lab: L[A, B])(fl: A => L[C, D], fr: B => R[C, D]): L[C, D]
  def rightFlatMap[A, B, C, D](rab: R[A, B])(fl: A => L[C, D], fr: B => R[C, D]): R[C, D]

  def leftBifunctor: Bifunctor[L] = new Bifunctor[L] {
    def bimap[A, B, C, D](fab: L[A, B])(f: A => C, g: B => D): L[C, D] =
      leftFlatMap(fab)(a => left(f(a)), b => right(g(b)))
  }

  def rightBifunctor: Bifunctor[R] = new Bifunctor[R] {
    def bimap[A, B, C, D](fab: R[A, B])(f: A => C, g: B => D): R[C, D] =
      rightFlatMap(fab)(a => left(f(a)), b => right(g(b)))
  }
}

trait Proc[F[_, _]] extends BiMonad[F, F] { self =>
  def pure[E, A](a: A): F[E, A]

  def raise[E, A](e: E): F[E, A]

  def foldWith[E, A, X, R](fa: F[E, A], h: E => F[X, R], f: A => F[X, R]): F[X, R]

  def fromEither[E, A](ea: Either[E, A]): F[E, A] = ea match {
    case Left(e)  => raise(e)
    case Right(a) => pure(a)
  }

  def fold[E, A, X, R](fa: F[E, A], h: E => R, f: A => R): F[X, R] =
    foldWith[E, A, X, R](fa, e => monad[X].pure(h(e)), a => monad[X].pure(f(a)))

  def handleWith[E, X, A](fa: F[E, A], h: E => F[X, A]): F[X, A] =
    foldWith(fa, h, monad[X].pure[A])

  def handle[E, X, A](fa: F[E, A], h: E => A): F[X, A] =
    handleWith[E, X, A](fa, e => monad[X].pure(h(e)))

  def left[A, B](a: A): F[A, B] = raise(a)

  def right[A, B](b: B): F[A, B] = pure(b)

  def leftFlatMap[A, B, C, D](lab: F[A, B])(fl: A => F[C, D], fr: B => F[C, D]): F[C, D] = foldWith(lab, fl, fr)

  def rightFlatMap[A, B, C, D](rab: F[A, B])(fl: A => F[C, D], fr: B => F[C, D]): F[C, D] = foldWith(rab, fl, fr)

  def monad[E]: Monad[F[E, *]] = new StackSafeMonad[F[E, *]] {
    def pure[A](x: A): F[E, A] = self.pure(x)

    def flatMap[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B] = foldWith[E, A, E, B](fa, raise, f)
  }

  val bifunctor: Bifunctor[F] = new Bifunctor[F] {
    def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
      foldWith[A, B, C, D](fab, a => raise(f(a)), b => pure(g(b)))
  }

  override def leftBifunctor: Bifunctor[F] = bifunctor

  override def rightBifunctor: Bifunctor[F] = bifunctor
}

trait ProcRec[F[_, _]] extends Proc[F] { self =>
  def foldRec[E, A, X, B](init: Either[E, A])(step: Either[E, A] => F[Either[E, X], Either[A, B]]): F[X, B]

  override def monad[E]: Monad[F[E, *]] = new Monad[F[E, *]] {
    def pure[A](x: A): F[E, A] = self.pure(x)

    def flatMap[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B] = foldWith[E, A, E, B](fa, raise, f)

    def tailRecM[A, B](a: A)(f: A => F[E, Either[A, B]]): F[E, B] =
      foldRec[Nothing, A, E, B](Right(a)) { ea => bifunctor.leftMap(f(ea.merge))(Right(_)) }
  }
}

class ProcSyntax[F[+_, +_], E, A](private val self: F[E, A]) extends AnyVal {
  def flatMap[E1 >: E, B](f: A => F[E1, B])(implicit F: Proc[F]): F[E1, B] =
    F.monad[E1].flatMap(self)(f)

  def map[B](f: A => B)(implicit F: Proc[F]): F[E, B] = F.monad[E].map(self)(f)

  def mapErr[X](f: E => X)(implicit F: Proc[F]): F[X, A] = F.bifunctor.leftMap(self)(f)

  def handleWith[X, A1 >: A](f: E => F[X, A1])(implicit F: Proc[F]): F[X, A1] = F.handleWith(self, f)

  def handle[A1 >: A](f: E => A1)(implicit F: Proc[F]): F[Nothing, A1] = F.handle(self, f)

  def foldWith[X, R](h: E => F[X, R], f: A => F[X, R])(implicit F: Proc[F]): F[X, R] = F.foldWith(self, h, f)

  def fold[R](h: E => R, f: A => R)(implicit F: Proc[F]): F[Nothing, R] = F.fold(self, h, f)

  def as[B](b: B)(implicit F: Proc[F]): F[E, B] = F.monad[E].as(self, b)

  def void(implicit F: Proc[F]): F[E, Unit] = F.monad.void(self)
}

object Proc extends ProcRecInstanceChain[Proc]

object ProcRec extends ProcRecInstanceChain[ProcRec]

trait ProcRecInstanceChain[TC[f[_, _]] >: ProcRec[f]] {
  implicit val eitherInstance: TC[Either] = new ProcRec[Either] {
    def pure[E, A](a: A): Either[E, A] = Right(a)

    def raise[E, A](e: E): Either[E, A] = Left(e)

    def foldWith[E, A, X, R](fa: Either[E, A], h: E => Either[X, R], f: A => Either[X, R]): Either[X, R] = fa.fold(h, f)

    def foldRec[E, A, X, B](init: Either[E, A])(
        step: Either[E, A] => Either[Either[E, X], Either[A, B]]
    ): Either[X, B] = {
      @tailrec def go(e: Either[E, A]): Either[X, B] = step(e) match {
        case Left(Left(e))   => go(Left(e))
        case Left(Right(x))  => Left(x)
        case Right(Left(a))  => go(Right(a))
        case Right(Right(b)) => Right(b)
      }

      go(init)
    }

    override def fromEither[E, A](ea: Either[E, A]): Either[E, A] = ea

    override def fold[E, A, X, R](fa: Either[E, A], h: E => R, f: A => R): Either[X, R] = Right(fa.fold(h, f))

    override def handle[E, X, A](fa: Either[E, A], h: E => A): Either[X, A] = Right(fa.fold(h, identity))

    override def monad[E]: Monad[Either[E, *]] = cats.instances.either.catsStdInstancesForEither

    override val bifunctor: Bifunctor[Either] = cats.instances.either.catsStdBitraverseForEither
  }
}
