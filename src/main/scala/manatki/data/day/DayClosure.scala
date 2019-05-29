package manatki.data.day

trait DayClosure[F[_], G[_], A] {
  def apply[B, C](fb: F[B])(f: (A, B) => C): G[C]
}
