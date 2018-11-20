package manatki

import cats.effect.{Async, Concurrent, IO}
import cats.effect.concurrent.Deferred
import fs2._
import cats.syntax.option._
import fs2.Stream.eval
import cats.syntax.applicativeError._

object fsfs {
  def zipWithHeader[F[_], A]: Pipe[F, A, (A, A)] =
    _.mapAccumulate(none[A]) {
      case (None, header)            => (header.some, none[(A, A)])
      case (ho @ Some(header), line) => (ho, (header -> line).some)
    }.collect { case (_, Some(x)) => x }

  //as requested by Dmitrii 2:52:11 19.11.2018 https://t.me/scala_ru/182611
  /** gives access to single termination event, allowing stream to shutdown gracefully */
  def withShutdown[F[_], A](f: F[Unit] => fs2.Stream[F, A])(implicit F: Concurrent[F], ev: Concurrent[IO]): fs2.Stream[F, A] = {
    val register = Deferred[IO, Unit].map { promise =>
      val shutdown = F.async[Unit](cb =>
        sys.addShutdownHook {
          cb(Right())
          promise.get.unsafeRunSync()
      })

      (promise, shutdown)
    }

    eval(F.liftIO(register)).flatMap {
      case (promise, shutdown) =>
        f(shutdown)
          .interruptWhen(shutdown.attempt)
          .onFinalize {
            F.liftIO(promise.complete())
          }
    }
  }
}
