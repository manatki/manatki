package manatki.concurrent

import zio.stream.ZStream
import zio.Chunk
import zio.ZIO
import zio.Semaphore
import MemoData._
import zio.Ref
import zio.Promise
import zio.UIO

sealed trait MemoData[-R, +E, +A] {
  def stream: ZStream[R, E, A] = this match {
    case End                   => ZStream.empty
    case Continue(chunk, next) => ZStream.fromChunk(chunk) ++ ZStream.fromEffect(next).flatMap(_.stream)
  }
}

object MemoData {
  type MemoPromise[-R, +E, +A] = ZIO[R, E, MemoData[R, E, A]]
  case object End                                                           extends MemoData[Any, Nothing, Nothing]
  case class Continue[R, E, A](chunk: Chunk[A], next: MemoPromise[R, E, A]) extends MemoData[R, E, A]

}

case class MemoTopic[R, E, A](
    start: MemoPromise[R, E, A],
    current: Ref[Promise[E, MemoData[R, E, A]]],
    lock: Semaphore
) {
  def push(data: Chunk[A]): UIO[Boolean] =
    lock
      .withPermit((for {
        promise <- Promise.make[E, MemoData[R, E, A]]
        prev    <- current.modify(prev => (prev, promise))
        result  <- prev.complete(UIO.succeed(Continue(data, promise.await)))
      } yield result).uninterruptible)

  def stop: UIO[Unit] = lock.withPermit(current.get.flatMap(_.complete(UIO.succeed(End))).unit)

  def stream: ZStream[R, E, A] = ZStream.fromEffect(start).flatMap(_.stream)
}

object MemoTopic {
  def create[R, E, A]: UIO[MemoTopic[R, E, A]] = for {
    promise <- Promise.make[E, MemoData[R, E, A]]
    sem     <- Semaphore.make(1)
    ref     <- Ref.make(promise)
  } yield MemoTopic(promise.await, ref, sem)
}
