package manatki.data
import cats.{Monad, StackSafeMonad}

sealed trait Freer[+F[_], A] {
  def flatMap[G[x] >: F[x], B](f: A => Freer[G, B]): Freer[G, B]
}

object Freer {
  def pure[A](a: A): Freer[Nothing, A] = Pure(a)
  def lift[F[_], A](fa: F[A]): Freer[F, A] = new Bind[F, A] {
    type Pin = A
    def head: F[A]              = fa
    def cont(a: A): Freer[F, A] = pure(a)
  }

  final case class Pure[A](a: A) extends Freer[Nothing, A] {
    def flatMap[G[_], B](f: A => Freer[G, B]) = f(a)
  }

  trait Bind[+F[_], A] extends Freer[F, A] { self =>
    type Pin
    def head: F[Pin]
    def cont(a: Pin): Freer[F, A]
    def flatMap[G[x] >: F[x], B](f: A => Freer[G, B]): Bind[G, B] = new Bind[G, B] {
      type Pin = self.Pin
      def head                                                               = self.head
      def cont(a: Pin)                                                       = self.cont(a).flatMap[G, B](f)
      override def flatMap[H[x] >: G[x], C](g: B => Freer[H, C]): Bind[H, C] = self.flatMap(a => f(a).flatMap(g))
    }
  }

  implicit def monadInstance[F[_]]: Monad[Freer[F, *]] = monadInstanceAny.asInstanceOf[MonadInstance[F]]

  private class MonadInstance[F[_]] extends StackSafeMonad[Freer[F, *]] {
    override def flatMap[A, B](fa: Freer[F, A])(f: A => Freer[F, B]): Freer[F, B] = fa.flatMap(f)
    override def pure[A](x: A): Freer[F, A]                                       = Freer.Pure(x)
  }

  private val monadInstanceAny: MonadInstance[Any] = new MonadInstance

}
