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
        (moveBack *> check).replicateA(k).map(_.forall(_ == sw))

    def checkK(k: Int): F[Boolean] = checkFor(k, true).map2(checkFor(k, false))(_ && _)

    def searchStart(cur: Int): F[Int] =
      checkK(cur).ifM(searchStart(cur * 2), searchBin(cur, cur * 2))
    def searchBin(from: Int, to: Int): F[Int] =
      if (to - from == 1) from.pure
      else {
        val mid = (from + to) / 2
        checkK(mid).ifM(searchBin(mid, to), searchBin(from, mid))
      }

    searchStart(1)
  }

  final case class CT(cur: Int, switches: BitSet)

  object cyclicTrainState extends CyclicTrain[State[CT, *]] {
    private def move(x: Int) =
      State.modify[CT] { ct =>
        val n = ct.switches.size
        ct.copy(cur = (ct.cur + x + n) % n)
      }
    def moveForward: State[CT, Unit] = move(1)
    def moveBack: State[CT, Unit]    = move(-1)
    def check: State[CT, Boolean]    = State.get[CT] map (ct => ct.switches(ct.cur))
    def switch(on: Boolean): State[CT, Unit] = State.modify[CT] { ct =>
      ct.copy(switches = if (on) ct.switches + ct.cur else ct.switches - ct.cur)
    }
  }
}
