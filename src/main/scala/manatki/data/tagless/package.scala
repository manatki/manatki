package manatki.data

package object tagless {
  type Algebra[P[_, _], A] = P[A, A]
}
