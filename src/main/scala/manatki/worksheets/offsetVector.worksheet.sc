import scala.collection.immutable.LongMap

final case class State[A](
    values: Map[A, State.Offset],
    offsets: LongMap[Set[A]] = LongMap.empty
) {
  import State.Offset
  def start(key: A, offset: Offset): State[A] =
    State(
      values = values.updated(key, offset),
      offsets = offsets.updated(offset, offsets.getOrElse(offset, Set.empty[A]) + key)
    )

  def finish(key: A, offset: Offset): (State[A], Option[Offset]) = {
    values.get(key) match {
      case None      => (this, None)
      case Some(off) =>
        val offVals    = offsets(off) - key
        val newOffsets = if (offVals.isEmpty) offsets - offset else offsets.updated(offset, offVals)
        val minOffset  = if (newOffsets.isEmpty) None else Some(newOffsets.firstKey)
        (State(values - key, newOffsets), minOffset)
    }
  }
}

object State {

  type Offset = Long

  def empty[A]: State[A] = State[A](Map.empty)
}
