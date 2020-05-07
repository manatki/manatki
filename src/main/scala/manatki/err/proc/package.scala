package manatki.err

package object proc {
  implicit def procSyntax[F[+_, +_], E, A](self: F[E, A]): ProcSyntax[F, E, A] = new ProcSyntax[F, E, A](self)

  def pure[F[_, _]] = new PureApp[F](true)
  class PureApp[F[_, _]](private val __ : Boolean) extends AnyVal {
    def apply[A](a: A)(implicit F: Proc[F]): F[Nothing, A] = F.pure(a)
  }

  def raise[F[_, _]] = new RaiseApp[F](true)
  class RaiseApp[F[_, _]](private val __ : Boolean) extends AnyVal {
    def apply[E](a: E)(implicit F: Proc[F]): F[E, Nothing] = F.raise(a)
  }
}
