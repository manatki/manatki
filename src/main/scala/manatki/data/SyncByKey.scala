package manatki.data

import cats.data.OptionT
import cats.~>
import monix.eval.{MVar, Task, TaskSemaphore}

//discussed with Kirill Shelopugin 2018-02-06 19:18
/** Synchronizes multiple tasks, sequencing actions for single key */
sealed abstract class SyncByKey[K] {
  protected def syncForKey(key: K): Task[TaskSemaphore]
  def sync[A](key: K)(task: Task[A]): Task[A] = syncForKey(key).flatMap(_.greenLight(task))
  def trans(key: K): Task ~> Task = new (Task ~> Task) {
    override def apply[A](fa: Task[A]): Task[A] = sync(key)(fa)
  }
}

object SyncByKey {
  /** creates growable synchronize map */
  def apply[K](): Task[SyncByKey[K]] =
    for (syncsVar <- MVar(Map.empty[K, TaskSemaphore]))
      yield new SyncByKey[K] {
        protected def syncForKey(key: K): Task[TaskSemaphore] = for {
          syncs <- syncsVar.take
          sync <- OptionT.fromOption[Task](syncs.get(key)).getOrElseF(TaskSemaphore(1))
          _ <- syncsVar.put(syncs + (key -> sync))
        } yield sync
      }

  /** creates synchronize map with fixed key set */
  def apply[K](keys: TraversableOnce[K]): Task[SyncByKey[K]] =
    for (syncs <- Task.traverse(keys)(i => TaskSemaphore(1).map(i -> _)))
      yield {
        val syncMap = syncs.toMap
        new SyncByKey[K] {
          protected def syncForKey(key: K): Task[TaskSemaphore] = Task.eval(syncMap(key))
        }
      }
}
