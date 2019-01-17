package manatki.data
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Ref, Semaphore}
import cats.implicits._

final case class Quirill[F[_]: Sync, A](lock: Semaphore[F], items: Ref[F, Vector[A]]) {
  def read(count: Int): F[Vector[A]] =
    items.modify(_.splitAt(count).swap) <* lock.releaseN(count)

  def trySend(item: A): F[Boolean] =
    lock.tryAcquire.ifM(items.update(_ :+ item) as true, false.pure[F])
}

object Quirill {
  def create[F[_]: Concurrent, A](size: Int): F[Quirill[F, A]] =
    Semaphore[F](size).map2(Ref[F].of(Vector[A]()))(apply(_, _))
}
