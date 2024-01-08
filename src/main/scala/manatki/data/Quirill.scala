package manatki.data
import cats.effect.std.Semaphore
import cats.effect.{Async, Concurrent, Ref, Sync}
import cats.implicits._

final case class Quirill[F[_]: Sync, A] private (lock: Semaphore[F], items: Ref[F, Vector[A]]) {
  private def put(item: A) = items.update(_ :+ item)

  def read(count: Int): F[Vector[A]] =
    items.modify(_.splitAt(count).swap).flatTap(v => lock.releaseN(v.size))

  def trySend(item: A): F[Boolean] = lock.tryAcquire.flatTap(put(item).whenA)

  def send(item: A): F[Unit] = lock.acquire *> put(item)
}

object Quirill {
  def create[F[_]: Async, A](size: Int): F[Quirill[F, A]] = createIn[F, F, A](size)

  def createIn[I[_]: Sync, F[_]: Async, A](size: Int): I[Quirill[F, A]] =
    Semaphore.in[I, F](size).map2(Ref.in[I, F, Vector[A]](Vector.empty))(apply(_, _))
}
