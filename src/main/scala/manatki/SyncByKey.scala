package manatki

import cats.arrow.FunctionK
import cats.~>
import monix.eval.{Coeval, MVar, Task, TaskSemaphore}


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
  def apply[K](): SyncByKey[K] = new SyncByKey[K] {
    private val syncsVar = MVar(Map.empty[K, TaskSemaphore])

    protected def syncForKey(key: K): Task[TaskSemaphore] = for {
      syncs <- syncsVar.take
      sync = syncs.getOrElse(key, TaskSemaphore(1))
      _ <- syncsVar.put(syncs + (key -> sync))
    } yield sync
  }

  /** creates synchronize map with fixed key set */
  def apply[K](keys: TraversableOnce[K]): SyncByKey[K] = new SyncByKey[K] {
    val syncs = keys.toIterator.map(_ -> TaskSemaphore(1)).toMap
    protected def syncForKey(key: K): Task[TaskSemaphore] = Task.eval(syncs(key))
  }
}
