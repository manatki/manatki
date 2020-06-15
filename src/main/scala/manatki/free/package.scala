package manatki

package object free {
  type Bud[-P <: Petal[Nothing], +E, +A] = Bud.T[P, E, A]
}
