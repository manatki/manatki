package manatki.data.tagless

import cats.Id
import cats.tagless.FunctorK
import tofu.syntax.funk.funK

abstract class ProfData[P[-_, +_]] {
  type T       = Layer[P]
  type Fold[a] = P[a, a]

  type FoldOps[f[_]]

  val foldOps: FoldOps[Fold]

  implicit val proTraverse: ProTraverse[P]

  implicit def layerFoldOps(l: Layer[P])(implicit F: FunctorK[FoldOps]): FoldOps[Id] =
    F.mapK(foldOps)(funK(folder => l.foldS(folder)))
}

abstract class ProfData1[P[-el, -input, +output]] {
  type T[+A] = Layer[P[A, -*, +*]]
  type Fold[A, a] = P[A, a, a]

  type FoldOps[A, f[_]]

  def foldOps[A]: FoldOps[A, Fold[A, *]]

  implicit def proTraverse[A]: ProTraverse[P[A, -*, +*]]

  implicit def layerFoldOps[A](l: Layer[P[A, -*, +*]])(implicit F: FunctorK[FoldOps[A, *[_]]]): FoldOps[A, Id] =
    F.mapK(foldOps)(funK(folder => l.foldS(folder)))
}
