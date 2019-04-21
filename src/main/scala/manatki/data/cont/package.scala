package manatki.data

package object cont {
  type ContXT[F[_], X, A] = ContX[F[X], A]
}
