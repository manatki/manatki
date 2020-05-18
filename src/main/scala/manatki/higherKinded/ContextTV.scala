package manatki.higherKinded


///**
//  * A ContextT with enabled variance
//  */
//trait ContextTV[F[+_], C[_[_]], +A]{
//  /** run computation, providing context */
//  def run[C1[x] <: C[x]](c: C1[ContextTV[F, C1, +*]]): F[A]
//}
