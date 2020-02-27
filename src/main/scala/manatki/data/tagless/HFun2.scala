package manatki.data.tagless

trait HFun2[-U[_[+_], _[+_]], +V[_[+_], _[+_]]] {
  def apply[F[+_], G[+_]](u: U[F, G]): V[F, G]
}
