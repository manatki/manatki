package manatki.data

package object cont {
  type ContET[F[_], X, A] = ContE[F[X], A]
  type ContTT[F[_], X, A] = Cont[F[X], A]
}
