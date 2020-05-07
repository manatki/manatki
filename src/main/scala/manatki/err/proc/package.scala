package manatki.err

package object proc {
  implicit def procSyntax[F[+_, +_], E, A](self: F[E, A]): ProcSyntax[F, E, A] = new ProcSyntax[F, E, A](self)

  def pure[F[_, _]] = Proc.pure[F]

  def raise[F[_, _]] = Proc.raise[F]
}
