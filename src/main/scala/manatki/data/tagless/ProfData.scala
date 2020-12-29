package manatki.data.tagless

import cats.{Id, Show}
import cats.tagless.FunctorK
import tofu.syntax.funk.funK

abstract class ProfData[P[-_, +_]] {
  type T        = Layer[P]
  type Fold[a]  = P[a, a]
  type Match[a] = P[T, a]

  type FoldOps[f[_]]

  val foldOps: FoldOps[Fold]

  implicit val foldFunctor: FunctorK[FoldOps]
  implicit val proTraverse: ProTraverse[P]

  lazy val mk: P[T, T] = ProCorep.construct[P]

  implicit def layerFoldOps(l: Layer[P])(implicit F: FunctorK[FoldOps]): FoldOps[Id] =
    F.mapK(foldOps)(funK(folder => l.foldS(folder)))

  implicit def show(implicit toStr: Fold[String]): Show[T] = _.foldS(toStr)
}

abstract class ProfData1[P[-el, -input, +output]] {
  type T[+A]      = Layer[P[A, -*, +*]]
  type Fold[A, a] = P[A, a, a]

  type FoldOps[A, f[_]]

  def foldOps[A]: FoldOps[A, Fold[A, *]]

  def make[A]: P[A, T[A], T[A]] = ProCorep.construct

  implicit def proTraverse[A]: ProTraverse[P[A, -*, +*]]
  implicit def opsFunctor[A]: FunctorK[FoldOps[A, *[_]]]

  implicit def layerFoldOps[A](l: Layer[P[A, -*, +*]])(implicit F: FunctorK[FoldOps[A, *[_]]]): FoldOps[A, Id] =
    opsFunctor[A].mapK(foldOps)(funK(folder => l.foldS(folder)))
}
