package manatki.syntax

import cats.effect.{Async, Concurrent}
import cats.syntax.apply._
import cats.syntax.flatMap._

import scala.concurrent.ExecutionContext

object async {
  implicit class AsyncOps[F[_], A](val fa: F[A]) extends AnyVal {
    /** shift execution to another context */
    // provided by @gurinderu in personal discussion 08.06.2018 11:08
    def shift(ec: ExecutionContext)(implicit F: Concurrent[F]): F[A] =
      F.start(Async.shift[F](ec) *> fa) >>= (_.join)
  }
}
