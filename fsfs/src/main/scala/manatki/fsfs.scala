package manatki

import cats.effect._
import cats.effect.concurrent.Deferred
import cats.effect.syntax.effect._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import fs2._

object fsfs {
  def zipWithHeader[F[_], A]: Pipe[F, A, (A, A)] =
    _.mapAccumulate(none[A]) {
      case (None, header)            => (header.some, none[(A, A)])
      case (ho @ Some(header), line) => (ho, (header -> line).some)
    }.collect { case (_, Some(x)) => x }

  //as requested by Dmitrii 2:52:11 19.11.2018 https://t.me/scala_ru/182611
  type Shutdown[F[_]] = F[F[Unit]]

  /** gives access to single termination event, allowing stream to shutdown gracefully */
  def shutdown[F[_]](implicit F: ConcurrentEffect[F]): F[Shutdown[F]] =
    for {
      termination <- Deferred[F, Unit]
      shutdown    <- Deferred[F, F[Unit]]
      hook        = shutdown.complete(termination.complete(())) *> termination.get
      _           <- F.delay(sys.addShutdownHook(hook.toIO.unsafeRunSync()))
    } yield shutdown.get
}
