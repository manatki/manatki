package manatki.concurrent

import zio.stream.ZStream
import zio.Chunk
import zio.ZIO
import zio.Semaphore
import MemoData._
import zio.Ref
import zio.Promise
import zio.UIO
import zio.IO

sealed trait MemoData[+E, +A] {
  def stream: ZStream[Any, E, A] = this match {
    case End                   => ZStream.empty
    case Continue(chunk, next) => ZStream.fromChunk(chunk) ++ ZStream.fromEffect(next).flatMap(_.stream)
  }
}

object MemoData {
  type MemoPromise[+E, +A] = IO[E, MemoData[E, A]]
  case object End                                                        extends MemoData[Nothing, Nothing]
  case class Continue[R, E, A](chunk: Chunk[A], next: MemoPromise[E, A]) extends MemoData[E, A]

}

case class MemoTopic[E, A](
    start: MemoPromise[E, A],
    current: Ref[Promise[E, MemoData[E, A]]],
    lock: Semaphore
) {

  def publishIO(data: IO[E, Chunk[A]]): UIO[Boolean] =
    lock
      .withPermit((for {
        promise <- Promise.make[E, MemoData[E, A]]
        prev    <- current.modify(prev => (prev, promise))
        result  <- prev.complete(data.map(Continue(_, promise.await)))
      } yield result).uninterruptible)

  def publish(data: Chunk[A]): UIO[Boolean] = publishIO(UIO.succeed(data))

  def stop: UIO[Unit] = lock.withPermit(current.get.flatMap(_.complete(UIO.succeed(End))).unit)

  def fail(err: E): UIO[Boolean] = publishIO(IO.fail(err))

  def stream: ZStream[Any, E, A] = ZStream.fromEffect(start).flatMap(_.stream)
}

object MemoTopic {
  def create[E, A]: UIO[MemoTopic[E, A]] = for {
    promise <- Promise.make[E, MemoData[E, A]]
    sem     <- Semaphore.make(1)
    ref     <- Ref.make(promise)
  } yield MemoTopic(promise.await, ref, sem)
}
