package manatki.data

import scala.annotation.tailrec

/**
  * Least recent use cache
  * as discussed with @lmnet89 2020-12-17 12:00 https://t.me/scala_ru/299317
  *
  * @param q queue of element in insertion order
  * @param cache map of cached values with reinsert count
  * @param lim maximum cache size
  * @tparam K cache key
  * @tparam V cached value type
  */
case class LRUCache[K, +V] private (
    q: Vector[K],
    cache: Map[K, (Long, V)],
    lim: Int
) {

  /** read from the cache if present */
  def get(k: K): Option[V] = cache.get(k).map(_._2)

  /** put new value to the cache */
  def put[V1 >: V](k: K, v: V1): LRUCache[K, V1] =
    cache.get(k) match {
      case None          => copy(q = q :+ k, cache = cache.updated(k, (0L, v))).fix
      case Some((rc, v)) => copy(cache = cache.updated(k, (rc + 1, v)))
    }

  def withLim(newLim: Int): LRUCache[K, V] = copy(lim = newLim).fix

  // fix the size of cache until it's <= min(lim, 0)
  @tailrec
  private def fix: LRUCache[K, V] = if (q.length <= lim) this
  else
    q match {
      case k +: rest =>
        cache(k) match {
          case (0, _)  => copy(q = rest, cache = cache - k)
          case (rc, v) => copy(q = rest :+ k, cache = cache.updated(k, (rc - 1L, v))).fix
        }
      case _         => this
    }
}

object LRUCache {
  def empty[K](lim: Int): LRUCache[K, Nothing] = LRUCache(Vector.empty, Map.empty, lim)
}
