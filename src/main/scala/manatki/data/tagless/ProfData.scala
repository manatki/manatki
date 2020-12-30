package manatki.data.tagless

import cats.{Id, Show}
import cats.tagless.FunctorK
import io.circe.{Encoder, Json}
import tofu.syntax.funk.funK

trait ProfDataBase {
  type P[-_, +_]
  type T              = Layer[P]
  type Fold[a]        = P[a, a]
  type FoldM[F[_], a] = P[a, F[a]]
  type Match[a]       = P[T, a]

  implicit val proTraverse: ProTraverse[P]
}

abstract class ProfData[P1[-_, +_]] extends ProfDataBase {
  type P[-x, +y] = P1[x, y]
  lazy val mk: P[T, T] = ProCorep.construct[P]

  implicit def show(implicit toStr: Fold[String]): Show[T]      = _.foldS(toStr)
  implicit def encoder(implicit toJson: Fold[Json]): Encoder[T] = _.foldS(toJson)

  def cotabulate[A] = new Cotabulate[P, A](proTraverse)
}

class Cotabulate[P[_, _], A](private val proTraverse: ProTraverse[P]) extends AnyVal {
  def apply[B](k: Rep[P, A] => B): P[A, B] = proTraverse.cotabulate(k)
}

trait ProfFoldOps extends ProfDataBase {
  type FoldOps[f[_]]

  val foldOps: FoldOps[Fold]
  implicit val foldFunctor: FunctorK[FoldOps]

  implicit def layerFoldOps(l: Layer[P]): FoldOps[Id] =
    foldFunctor.mapK(foldOps)(funK(folder => l.foldS(folder)))
}

trait ProfMatchOps extends ProfDataBase {
  type MatchOps[f[_]]

  val matchOps: MatchOps[Match]
  implicit val matchFunctor: FunctorK[MatchOps]

  implicit def layerMatchOps(l: Layer[P]): MatchOps[Id] =
    matchFunctor.mapK(matchOps)(funK(matcher => l.unpack(matcher)))
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
