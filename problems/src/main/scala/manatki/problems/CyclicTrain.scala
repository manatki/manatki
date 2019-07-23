package manatki.problems

import cats.Monad
import cats.data.State
import cats.syntax.applicative._
import cats.syntax.monad._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.apply._
import monocle.macros.Lenses

import scala.collection.immutable.BitSet

trait CyclicTrain[F[_]] {
  def moveForward: F[Unit]
  def moveBack: F[Unit]
  def check: F[Boolean]
  def switch(on: Boolean): F[Unit]
}

object CyclicTrain {

  def countWagons[F[_]: Monad](train: CyclicTrain[F]): F[Int] = {
    import train._
    def checkFor(k: Int, sw: Boolean): F[Boolean] =
      (moveForward *> switch(sw)).replicateA(k) *>
        moveBack.replicateA(k) *>
        (check <* moveBack).replicateA(k).map(_.forall(_ == sw)) <*
        moveForward.replicateA(k)

    def checkK(k: Int): F[Boolean] = checkFor(k, true).map2(checkFor(k, false))(_ && _)

    def searchStart(from: Int, to: Int): F[Int] = checkK(to).ifM(searchBin(from, to), searchStart(to, to * 2))
    def searchBin(from: Int, to: Int): F[Int] =
      if (to - from == 1) to.pure
      else {
        val mid = (from + to) / 2
        checkK(mid).ifM(searchBin(from, mid), searchBin(mid, to))
      }

    searchStart(0, 1)
  }

  type CT = Vector[Boolean]

  object cyclicTrainState extends CyclicTrain[State[CT, *]] {
    def moveForward: State[CT, Unit]         = State.modify { case h +: rest => rest :+ h }
    def moveBack: State[CT, Unit]            = State.modify { case rest :+ l => l +: rest }
    def check: State[CT, Boolean]            = State.get map (_.head)
    def switch(on: Boolean): State[CT, Unit] = State.modify(_.updated(0, on))
  }
}
