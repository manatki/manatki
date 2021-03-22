package manatki.fsfs

import fs2._

object Pipes {
  /** convert pipe to lazy which is applied only when source stream is non-empty */
  // requested by @rudogma 22.03.2021 https://t.me/scala_ru/306591
  def lzy[F[_], A, B](p: Pipe[F, A, B])(s: Stream[F, A]): Stream[F, B] =
    s.pull.unconsNonEmpty.flatMap {
      case None                => Pull.done
      case Some(first -> next) => p(Stream.chunk(first) ++ next).pull.echo
    }.stream
}
