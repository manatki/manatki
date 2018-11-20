package manatki
import java.time.{Instant, ZoneId, ZonedDateTime}

import cats.effect.{Concurrent, IO, Sync, Timer}
import fs2.Stream.eval

import cats.effect._
import cats.effect.concurrent.Deferred
import cats.effect.syntax.effect._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.Stream.eval
import cats.instances.option._
import cats.syntax.foldable._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.concurrent.duration.MILLISECONDS

object TerminationUsage {
  implicit val iotimer = IO.timer(global)
  implicit val cs      = IO.contextShift(global)

  def stream[F[_]: Concurrent](sd: F[F[Unit]])(implicit timer: Timer[F]) =
    fs2.Stream
      .awakeEvery[F](1 seconds)
      .as(None)
      .merge(eval(sd.map(Some(_))))
      .evalMap { evt =>
        timer.clock.realTime(MILLISECONDS).flatMap { millis =>
          val now  = ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneId.systemDefault())
          val mess = evt.fold("normal event")(_ => "termination event")
          Sync[F].delay(println(s"$mess at $now")) *> evt.sequence_
        }
      }

  def main(args: Array[String]): Unit = {
    fsfs.shutdown[IO].flatMap(stream[IO](_).compile.drain).unsafeRunSync()
  }
}
