package manatki

import cats.{Functor, Order}
import cats.effect._
import cats.effect.concurrent.Deferred
import cats.effect.syntax.effect._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import fs2._

package object fsfs {
  def zipWithHeader[F[_], A]: Pipe[F, A, (A, A)] =
    _.mapAccumulate(none[A]) {
      case (None, header)            => (header.some, none[(A, A)])
      case (ho @ Some(header), line) => (ho, (header -> line).some)
    }.collect { case (_, Some(x)) => x }

  //as requested by Dmitrii 2:52:11 19.11.2018 https://t.me/scala_ru/182611
  //see test/ShutdownApp
  type Shutdown[F[_]] = F[F[Unit]]

  /** gives access to single termination event, allowing stream to shutdown gracefully */
  def shutdown[F[_]](implicit F: ConcurrentEffect[F]): F[Shutdown[F]] =
    for {
      termination <- Deferred[F, Unit]
      shutdown    <- Deferred[F, F[Unit]]
      hook         = shutdown.complete(termination.complete(())) *> termination.get
      _           <- F.delay(sys.addShutdownHook(hook.toIO.unsafeRunSync()))
    } yield shutdown.get

  // as requested by S.Mukhorovsky
  /** performs some action after first element received */
  implicit class FS2ManatkiStreamOps[F[_], A](private val stream: fs2.Stream[F, A]) {
    def doAfterHead(action: A => F[Unit])(implicit F: Functor[F]): fs2.Stream[F, A] =
      stream.pull.uncons1.flatMap { opt =>
        opt
          .map { case (head, tail) =>
            fs2.Stream.eval(action(head) as head) ++ tail
          }
          .getOrElse(fs2.Stream.empty)
          .pull
          .echo
      }.stream

    final case class GBW[K, +V](inScope: Boolean = false, vals: Map[K, V] = Map.empty[K, V], include: Boolean = false)

    //requested by @curiosady in @scala_learn 20.02.2021 13:49 https://t.me/scala_learn/22768
    def groupBetweenWith[K, V](bounds: PartialFunction[A, Boolean])(kv: A => (K, V))(
        agg: (V, V) => V
    ): fs2.Stream[F, Map[K, V]] =
      stream
        .scan(GBW[K, V]()) {
          case (GBW(false, m, _), a) if bounds.lift(a) == Some(true) => GBW(inScope = true)
          case (GBW(true, m, _), a) if bounds.lift(a) == Some(false) => GBW(vals = m, include = true)
          case (state, a) if bounds.isDefinedAt(a)                   => state
          case (GBW(false, m, _), _)                                 => GBW()
          case (GBW(true, m, _), a)                                  =>
            val (k, v) = kv(a)
            GBW(true, m.updatedWith(k)(ov => Some(ov.fold(v)(agg(v, _)))))
        }
        .filter(_.include)
        .map(_.vals)
  }

}
