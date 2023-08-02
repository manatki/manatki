package manatki.data

import cats._
import cats.syntax.all._
import scala.annotation.unused

/** A multilevel collection that can be used instead of Map or List inside broadcasrting handler set We know exactly how
	* it branches, so we can add another level of parallelization (.start) at level so broadcasting can become a little
	* bit more fair
	*/
sealed trait MultiLevelList[+A] extends Iterable[A] {
	def :+[A1 >: A](a: A1): MultiLevelList[A1]

	def -(key: Long): MultiLevelList[A]

	def counter: Long
}

object MultiLevelList {
	val BranchingFactor = 8L

	val empty: MultiLevelList[Nothing] = new Impl(Branch(Array()), 0, Up(Zero()))

	class Impl[+A, L](data: Branch[A, L], val counter: Long, level: Up[L]) extends MultiLevelList[A] {
		def iterator: Iterator[A] = iteratorOf(data)

		private def upgrade: Impl[A, Up[L]] = new Impl(Branch(Array(data)), counter, Up(level.self))

		final def :+[A1 >: A](a: A1): MultiLevelList[A1] =
			if (counter < level.maxCounter) upgrade :+ a
			else new Impl(push(data, a, counter, level.down), counter + 1, level)

		def -(key: Long): MultiLevelList[A] = new Impl(asBranch(remove(data, key, level)), counter, level)
	}

	/** a int substitution for knowing how deep collection is
		*/
	private sealed trait Level[L] {
		self: L =>
		def maxCounter: Long

		def self: L with Level[L] = self
	}

	/** representation of depth = 0
		*/
	private case class Zero() extends Level[Zero] {
		def maxCounter: Long = 1
	}

	/** representaion of depth = down + 1
		*/
	private case class Up[L](down: Level[L]) extends Level[Up[L]] {
		val maxCounter: Long = down.maxCounter * BranchingFactor
	}

	/** data cells and branches, indexed with depth
		*/
	private sealed trait Data[+A, L]

	private final case object Empty extends Data[Nothing, Zero]

	private final case class Value[+A](value: A) extends Data[A, Zero]

	private final case class Branch[+A, L](children: Array[_ <: Data[A, L]]) extends Data[A, Up[L]]

	private def singleton[A, L](a: A, level: Level[L]): Data[A, L] = level match {
		case Zero()    => Value(a)
		case Up(inner) => Branch(Array(singleton(a, inner)))
	}

	private def iteratorOf[A, L](data: Data[A, L]): Iterator[A] = data match {
		case Value(value)     => Iterator.single(value)
		case Branch(children) => children.iterator.flatMap(iteratorOf)
		case Empty            => Iterator.empty
	}

	private def asBranch[A, L](data: Data[A, Up[L]]): Branch[A, L] = data match {
		case b: Branch[A, inner] => b
	}

	private def push[A, L](branch: Branch[A, L], a: A, counter: Long, level: Level[L]): Branch[A, L] =
		level match {
			case up: Up[inner] if counter < branch.children.length * level.maxCounter =>
				val childIx                    = (counter / level.maxCounter).toInt
				val oldChild                   = asBranch(branch.children(childIx))
				val newChild: Branch[A, inner] = push(oldChild, a, counter, up.down)
				Branch(branch.children.updated(childIx, newChild))
			case _ => Branch(branch.children :+ singleton(a, level))
		}

	private def remove[A, L](data: Data[A, L], key: Long, level: Level[L]): Data[A, L] =
		level match {
			case _: Zero => Empty
			case up: Up[inner] =>
				val branch                   = asBranch(data)
				val childIx                  = (key / level.maxCounter).toInt
				val newChild: Data[A, inner] = remove(branch.children(childIx), key % level.maxCounter, up.down)
				Branch(branch.children.updated(childIx, newChild))
		}
}
