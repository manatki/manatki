package manatki.err

trait LocalT[+F[+_, +_], C, +E, +A] {
  def run[F1[+e, +a] >: F[e, a]](context: CtxLoc[C, F1]): F1[E, A]


}
