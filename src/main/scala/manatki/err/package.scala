package manatki

package object err {
  type Attempt[-X <: Err, +A] = Attempt.Attempt[X, A]
}
