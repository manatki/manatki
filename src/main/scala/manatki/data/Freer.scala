package manatki.data
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.{Applicative, Eval, Foldable, Functor, Monad, StackSafeMonad, Traverse}
import manatki.free.FunK
import tofu.syntax.monadic._

import scala.annotation.tailrec

sealed trait Freer[+F[_], A] {
  def flatMap[G[x] >: F[x], B](f: A => Freer[G, B]): Freer[G, B]
  def foldMap[G[_]: Monad](f: FunK[F, G]): G[A] = this.tailRecM {
    case Freer.Pure(a)               => a.asRight.pure[G]
    case bind: Freer.Bind[F, pin, A] => f(bind.head).map(a => bind.cont(a).asLeft)
  }
  // fold map for stack safe monads
  def foldMapL[G[_]: Monad](f: FunK[F, G]): G[A] = this match {
    case Freer.Pure(a)               => a.pure[G]
    case bind: Freer.Bind[F, pin, A] => f(bind.head).flatMap(pin => bind.cont(pin).foldMapL(f))
  }
  def mapK[G[_]](f: FunK[F, G]): Freer[G, A]

}

object Freer extends FreerInstances {
  val unit: Freer[Nothing, Unit] = pure(())

  def pure[A](a: A): Freer[Nothing, A] = Pure(a)
  def lift[F[_], A](fa: F[A]): Freer[F, A] = new Bind[F, A, A](fa) {
    def cont(a: A): Freer[F, A] = pure(a)
  }
  def roll[F[_], A](fa: F[Freer[F, A]]): Freer[F, A] = new Bind[F, Freer[F, A], A](fa) {
    def cont(a: Freer[F, A]): Freer[F, A] = a
  }
  implicit class FreerOps[F[_], A](private val self: Freer[F, A]) extends AnyVal {
    def runTailRec(implicit F: Monad[F]) = self.foldMap(FunK.id)

    final def fold[B](zero: A => B, suc: F[Freer[F, A]] => B)(implicit F: Functor[F]): B = self match {
      case Pure(a)               => zero(a)
      case bind: Bind[F, pin, A] => suc(bind.head.map(bind.cont))
    }

    @tailrec final def go(f: F[Freer[F, A]] => Freer[F, A])(implicit F: Functor[F]): A = self match {
      case Pure(a)               => a
      case bind: Bind[F, pin, A] => f(bind.head.map(bind.cont)).go(f)
    }
  }

  final case class Pure[A](a: A) extends Freer[Nothing, A] {
    def flatMap[G[_], B](f: A => Freer[G, B])                    = f(a)
    def mapK[G[_]](f: FunK[Nothing, G]): Freer[G, A]             = this
    override def foldMap[G[_]: Monad](f: FunK[Nothing, G]): G[A] = a.pure[G]
  }

  abstract class Bind[+F[_], Pin, A](val head: F[Pin]) extends Freer[F, A] { self =>
    def cont(a: Pin): Freer[F, A]

    def flatMap[G[x] >: F[x], B](f: A => Freer[G, B]): Bind[G, Pin, B] = new Bind[G, Pin, B](self.head) {
      def cont(a: Pin)                                                            = self.cont(a).flatMap[G, B](f)
      override def flatMap[H[x] >: G[x], C](g: B => Freer[H, C]): Bind[H, Pin, C] = self.flatMap(a => f(a).flatMap(g))
    }

    def mapK[G[_]](f: FunK[F, G]): Freer[G, A] = new Bind[G, Pin, A](f(self.head)) {
      def cont(a: Pin): Freer[G, A] = self.cont(a).mapK(f)
    }
  }

  implicit def monadInstance[F[_]]: Monad[Freer[F, *]] = monadInstanceAny.asInstanceOf[MonadInstance[F]]

  implicit def traverseInstance[F[_]: Traverse]: Traverse[Freer[F, *]] = new FreeTraverse

  private class MonadInstance[F[_]] extends StackSafeMonad[Freer[F, *]] {
    override def flatMap[A, B](fa: Freer[F, A])(f: A => Freer[F, B]): Freer[F, B] = fa.flatMap(f)
    override def pure[A](x: A): Freer[F, A]                                       = Freer.Pure(x)
  }

  private val monadInstanceAny: MonadInstance[Any] = new MonadInstance
}

trait FreerInstances {
  final implicit def foldableInstance[F[_]: Foldable]: Foldable[Freer[F, *]] = new FreeFoldable
}

class FreeFoldable[F[_]: Foldable] extends Foldable[Freer[F, *]] {
  final def foldLeft[A, B](fa: Freer[F, A], b: B)(f: (B, A) => B): B =
    foldRight[A, Eval[B] => Eval[B]](fa, Eval.now(identity))(
      (a, ef) => Eval.later(eb => ef.flatMap(u => u(eb.map(b => f(b, a)))))
    ).value(Eval.now(b)).value

  final def foldRight[A, B](fa: Freer[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
    case Freer.Pure(a) => f(a, lb)
    case bind: Freer.Bind[F, pin, A] =>
      Eval.defer(bind.head.foldRight(lb)((pin, lb1) => foldRight(bind.cont(pin), lb1)(f)))
  }
}

class FreeTraverse[F[_]: Traverse] extends FreeFoldable[F] with Traverse[Freer[F, *]] {
  final def traverse[G[_]: Applicative, A, B](fa: Freer[F, A])(f: A => G[B]): G[Freer[F, B]] =
    fa match {
      case Freer.Pure(a)               => f(a).map(Freer.pure)
      case bind: Freer.Bind[F, pin, A] => bind.head.traverse(pin => traverse(bind.cont(pin))(f)).map(Freer.roll)
    }
}
