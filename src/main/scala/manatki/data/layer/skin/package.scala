package manatki.data.layer

package object skin {
  type Algebra[P[_, _], A] = P[A, A]

  type Lst[I] = Skin.Fix[ListP[I, *, *]]
}
