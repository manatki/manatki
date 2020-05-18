package manatki.poc

class Uns {
  type Cov[+A]     = (A, A)
  type Contrav[-A] = A => String

  case class Applied[F[+_], +A](value: F[A])

  type App[F[+_], A] = Applied[F, A]


//  def run[C[f[_], A], F[_]](fa: C[F, Nothing] => C[F, String])
}
