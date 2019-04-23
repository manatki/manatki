package manatki.data

package object cont {
  type ContXT[F[_], X, A] = ContX[F[X], A]
  type ContET[F[_], X, A] = Cont[F[X], A]
}
