package manatki.data

import cats.data.StateT
import cats.effect._
import cats.effect.concurrent.{Deferred, MVar}
import cats.effect.syntax.bracket._
import cats.effect.syntax.concurrent._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monad._

final case class Daemon[F[_]](process: Fiber[F, Unit], exit: F[Daemon.Exit]) {
  def bindTo(daemon: Daemon[F])(implicit F: Concurrent[F]): F[Unit] =
    (daemon.exit *> process.cancel).start.void

  def kill = process.cancel
}

/** Probably Infinite processes */
object Daemon {
  type Exit = ExitCase[Throwable]

  def apply[F[_]: Concurrent](process: F[Unit]): F[Daemon[F]] =
    for {
      exit  <- Deferred[F, Exit]
      fiber <- process.guaranteeCase(exit.complete).start
    } yield Daemon(fiber, exit.get)

  def repeat[F[_]: Concurrent](step: F[Unit]): F[Daemon[F]] =
    apply(step.iterateWhile(_ => true))

  def iterate[F[_]: Concurrent, A](init: A)(step: A => F[A]): F[Daemon[F]] =
    apply(init.iterateWhileM(step)(_ => true).void)

  def state[F[_]: Concurrent, S](init: S)(state: StateT[F, S, Unit]): F[Daemon[F]] =
    iterate(init)(state.runS)

  def resource[F[_]: Concurrent, A](daemon: F[Daemon[F]]): Resource[F, Daemon[F]] =
    Resource.make(daemon)(_.process.cancel)
}

final class Actor[F[_], A] private (queue: MVar[F, A], val daemon: Daemon[F]) {

  /** fire message waiting for receive */
  def !!(message: A): F[Unit] = queue.put(message)

  /** fire and forget */
  def !(message: A)(implicit F: Concurrent[F]): F[Fiber[F, Unit]] = queue.put(message).start

  /** ask pattern with error handling */
  def ??[B](make: (Either[Throwable, B] => Unit) => A)(implicit F: Concurrent[F]): F[B] =
    F.asyncF(cb => !!(make(cb)))

  /** ask pattern without error handling */
  def ?[B](make: (B => Unit) => A)(implicit F: Concurrent[F]): F[B] =
    F.asyncF(cb => !!(make(b => cb(Right(b)))))

  def onStop(action: F[Unit])(implicit F: Concurrent[F]): F[Fiber[F, Unit]] = watch(_ => action)

  def watch(action: Daemon.Exit => F[Unit])(implicit F: Concurrent[F]): F[Fiber[F, Unit]] =
    (daemon.exit >>= action).start

  def send(message: A)(implicit F: Concurrent[F]): F[Unit] = this !! message

  def tell(message: A)(implicit F: Concurrent[F]): F[Fiber[F, Unit]] = this ! message
}

object Actor {
  final case class Behavior[F[_], A](receive: A => F[Option[Behavior[F, A]]])

  def spawn[F[_]: Concurrent, A](behavior: Behavior[F, A]): F[Actor[F, A]] =
    for {
      mvar <- MVar.empty[F, A]
      daemon <- Daemon.iterate(behavior)(b =>
        for {
          a <- mvar.take
          r <- behavior.receive(a)
        } yield r.getOrElse(b))
    } yield new Actor(mvar, daemon)

  def apply[F[_]: Concurrent, A](receive: A => F[Unit]): F[Actor[F, A]] =
    for {
      mvar   <- MVar.empty[F, A]
      daemon <- Daemon.repeat(mvar.take >>= receive)
    } yield new Actor(mvar, daemon)

  def sync[F[_]: Concurrent, A](receive: A => Unit): F[Actor[F, A]] = apply(a => receive(a).pure[F])

  def syncSupervise[F[_]: Concurrent, A](receive: A => Unit)(strategy: Throwable => F[Unit]): F[Actor[F, A]] =
    apply(a => Sync[F].delay(receive(a)).handleErrorWith(strategy))
}

