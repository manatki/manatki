package manatki
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING}
import java.nio.file.{Files, Paths}

import cats.Show
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import manatki.data.Quirill
import monix.eval.{Task, TaskApp}

import scala.concurrent.duration._
import scala.io.StdIn

object QuirillApp extends TaskApp {
  val out = Files.newBufferedWriter(Paths.get("output.txt"), TRUNCATE_EXISTING, CREATE)

  def log[F[_]](mess: String)(implicit F: Sync[F]): F[Unit]  = F.delay { out.write(mess); out.newLine(); out.flush() }
  def sleep[F[_]](t: Int)(implicit timer: Timer[F]): F[Unit] = timer.sleep(t seconds)

  def consumer[F[_]: Sync, A: Show](quirill: Quirill[F, A], batchSize: Int)(implicit t: Timer[F]): F[Unit] =
    (quirill.read(batchSize) >>= (v => log[F](show"took $v"))) *> sleep(1)

  def producer[F[_]: Sync: Timer, A: Show](elem: A, quirill: Quirill[F, A]): F[Unit] =
    quirill.trySend(elem).ifM(log[F](show"sent $elem"), log[F](show"could not send $elem")) *> sleep(1)

  def startResource[F[_]: Concurrent, A](fa: F[A], name: String = "") =
    Resource.make(fa.start)(_.cancel <* log[F](show"$name canceled"))

  def process[F[_]: Concurrent: Timer, A: Show](queueSize: Int, batchSize: Int, producers: List[A]) = {
    def makeAll(quirill: Quirill[F, A]) =
      for {
        cons  <- startResource(consumer(quirill, batchSize).foreverM[Unit], show"consumer $batchSize")
        prods <- producers.traverse(elem => startResource(producer(elem, quirill).foreverM[Unit], show"producer $elem"))
      } yield cons :: prods

    for {
      quirill <- Quirill.create[F, A](queueSize)
      _       <- makeAll(quirill).use(_.traverse_(_.join))
    } yield ()
  }

  def parseInput[F[_]](implicit F: Sync[F]): F[(Int, Int, List[Int])] =
    for {
      input                    <- F.delay(StdIn.readLine())
      parts                    = input.filter(s => s == ' ' || s.isDigit).split(" ").filter(_.nonEmpty)
      Array(qs, bs, vals @ _*) <- F.delay(parts.map(_.toInt))
    } yield (qs, bs, vals.toList)

  def run[F[_]: Concurrent: Timer]: F[Unit] =
    none[Fiber[F, Unit]]
      .iterateForeverM[F, Unit] { prev =>
        for {
          f              <- parseInput[F].start
          (qs, bs, vals) <- f.join.guarantee(prev.traverse_[F, Unit](_.cancel))
          f              <- process[F, Int](qs, bs, vals).start
        } yield f.some
      }

  def run(args: List[String]): Task[ExitCode] = run[Task].executeWithOptions(_.enableAutoCancelableRunLoops) as ExitCode.Success
}
