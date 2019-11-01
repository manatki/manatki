package manatki
package data

import cats.arrow.FunctionK
import cats.effect.concurrent.Semaphore
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Functor, Monad, ~>}
import monocle.function.all._
import tofu.concurrent._
import tofu.concurrent.syntax.ref._
import manatki.syntax.functionK
import tofu.optics.Property
import tofu.optics.functions.mapItem

//discussed with Kirill Shelopugin 2018-02-06 19:18
/** Synchronizes multiple tasks, sequencing actions for single key */
object SyncByKey {
  type SyncByKey[F[_], K] = K => F ~> F
  type State[F[_], K]     = Map[K, Semaphore[F]]

  /** creates growable synchronize map */
  def apply[I[_]: Functor: MakeRef[*[_], F], F[_]: Monad: Semaphores, K]: I[SyncByKey[F, K]] =
    for (ref <- MakeRef[I, F].of(Map(): State[F, K]))
      yield (key: K) => impl(ref.optimisticModify(mapItem(key))(MakeSemaphore[F, F].of(1))(identity))

  /** creates synchronize map with fixed key set */
  def apply[I[_]: Applicative: MakeSemaphore[*[_], F], F[_]: Monad, K](keys: List[K]): I[SyncByKey[F, K]] =
    for (map <- keys.traverse(k => MakeSemaphore[I, F].of(1).tupleLeft(k)).map(_.toMap))
      yield (key: K) => map.get(key).fold(FunctionK.id[F])(s => functionK[F](s.withPermit))

  private def impl[F[_]: Monad, K](permit: F[Semaphore[F]]): F ~> F =
    functionK[F](fa => permit.flatMap(_.withPermit(fa)))

}
