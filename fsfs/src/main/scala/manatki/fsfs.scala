package manatki

import fs2._
import cats.syntax.option._

object fsfs {
  def zipWithHeader[F[_], A]: Pipe[F, A, (A, A)] =
    _.mapAccumulate(none[A]) {
      case (None, header) => (header.some, none[(A, A)])
      case (ho@Some(header), line) => (ho, (header -> line).some)
    }.collect { case (_, Some(x)) => x }
}
