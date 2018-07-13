package manatki
import akka.actor.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import cats.Monad
import cats.effect.{Async, ConcurrentEffect, IO, Timer}
import cats.effect.concurrent.{MVar, Ref}
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.effect.syntax.effect._
import cats.effect.syntax.concurrent._
import cats.instances.vector._
import manatki.AsyncBench._
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


trait AsyncBench {


  def run: Future[Vector[Long]]
  val n = 100000
  val w = 100

  def next(x: Int): Int = if (x % 2 == 0) x / 2 else x * 3 + 1

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(Await.result(run, Duration.Inf))
    val dur = (System.currentTimeMillis() - start).millis
    println(dur)
    system.terminate()
  }
}

object AsyncBench {
  implicit val system = ActorSystem()
  implicit val timeout = Timeout(1000 second)
  implicit val sched = system.scheduler
  implicit val scheduler = Scheduler(system.dispatcher)
  implicit val timer: Timer[IO] = IO.timer(system.dispatcher)
}

object PureBench extends AsyncBench {
  def collatz(counts: Vector[Long], x: Int): Vector[Long] =
    if (x == 1) counts else {
      val k = x % w
      val cnt = counts(k) + 1
      collatz(counts.updated(k, cnt), next(x))
    }

  def run = Future((1 to n).foldLeft(Vector.fill(w)(0L))(collatz))
}

object ImmutableActorBench extends AsyncBench {
  sealed trait Worker

  object Worker {
    final case class Calc(num: Int) extends Worker
    final case class Init(workers: IndexedSeq[ActorRef[Worker]], finalizer: ActorRef[Finalizer]) extends Worker
    final case class Result(count: ActorRef[Long]) extends Worker

    def apply(): Behavior[Worker] =
      Behaviors.receiveMessagePartial {
        case Init(workers, finalizer) => initialized(0, workers, finalizer)
      }

    private def initialized(count: Long, workers: IndexedSeq[ActorRef[Worker]], finalizer: ActorRef[Finalizer]): Behavior[Worker] =
      Behaviors.receiveMessagePartial {
        case Calc(1) =>
          finalizer ! Finalizer.Done
          initialized(count + 1, workers, finalizer)
        case Calc(x) =>
          val nxt = next(x)
          workers(nxt % w) ! Calc(nxt)
          initialized(count + 1, workers, finalizer)
        case Result(cnt) =>
          cnt ! count
          Behaviors.same
      }


  }

  sealed trait Finalizer

  object Finalizer {
    case object Done extends Finalizer
    final case class Ready(ready: ActorRef[Unit]) extends Finalizer

    def apply(done: Int = 0): Behavior[Finalizer] =
      Behaviors.receiveMessage {
        case Done =>
          apply(done + 1)
        case Ready(ready) if done == n =>
          ready ! {}
          Behaviors.same
        case Ready(ready) => waitReady(ready, done)
      }

    private def waitReady(ready: ActorRef[Unit], done: Int): Behavior[Finalizer] =
      Behaviors.receiveMessagePartial {
        case Done =>
          if (done == n - 1) ready ! {}
          waitReady(ready, done + 1)
      }
  }
  override def run: Future[Vector[Long]] = {
    val workers = for (i <- 0 to w) yield system.spawnAnonymous(Worker())
    val finalizer = system.spawnAnonymous(Finalizer())
    for (worker <- workers) worker ! Worker.Init(workers, finalizer)
    for (i <- 1 to n) workers(i % w) ! Worker.Calc(i)
    for {
      _ <- finalizer ? Finalizer.Ready
      counts <- Future.traverse(workers)(_ ? Worker.Result)
    } yield counts.toVector
  }
}

abstract class AsyncMutStateBench[F[_] : ConcurrentEffect] extends AsyncBench {
  def collatz(counts: Vector[MVar[F, Long]], x: Int): F[Unit] =
    if (x == 1) Monad[F].unit else {
      val k = x % w
      for {
        prev <- counts(k).take
        _ <- counts(k).put(prev + 1)
        _ <- collatz(counts, next(x))
      } yield ()
    }

  def exec(counts: Vector[MVar[F, Long]]): F[Unit] =
    List.range(1, n + 1).traverse_(collatz(counts, _))


  override def run: Future[Vector[Long]] =
    (for {
      counts <- Vector.fill(w)(MVar[F].of(0L)).sequence[F, MVar[F, Long]]
      _ <- exec(counts)
      result <- counts.traverse(_.read)
    } yield result).toIO.unsafeToFuture()
}

abstract class AsyncMutFiberBench[F[_] : ConcurrentEffect] extends AsyncMutStateBench[F] {
  override def exec(counts: Vector[MVar[F, Long]]): F[Unit] = for {
    fibers <- List.range(1, n + 1).traverse(collatz(counts, _).start)
    _ <- fibers.traverse(_.join)
  } yield ()
}

//abstract class AsyncBenchImpl[F[_] : ConcurrentEffect] extends AsyncBench {
//  def collatz(x: Int): F[Unit] =
//    if (x == 1) Monad[F].unit else {
//      val k = x % w
//      for {
//        prev <- counts(k).take
//        _ <- counts(k).put(prev + 1)
//        _ <- collatz(counts, next(x))
//      } yield ()
//    }
//
//  def exec(counts: Vector[MVar[F, Long]]): F[Unit] =
//    List.range(1, n + 1).traverse_(collatz(counts, _))
//
//
//  override def run: Future[Vector[Long]] =
//    (for {
//      counts <- Vector.fill(w)(MVar[F].of(0L)).sequence[F, MVar[F, Long]]
//      _ <- exec(counts)
//      result <- counts.traverse(_.read)
//    } yield result).toIO.unsafeToFuture()
//}


object IOBench extends AsyncMutStateBench[IO]
object IOBenchFiber extends AsyncMutFiberBench[IO]
object TaskBench extends AsyncMutStateBench[Task]
object TaskBenchFiber extends AsyncMutFiberBench[Task]

