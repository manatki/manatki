package manatki.data.simple

import cats.data.{OptionT, Tuple2K}
import cats.free.Cofree
import cats.implicits._
import cats.{Eval, Id}

object FreeTree extends App {
  type TreeF[A]   = OptionT[Tuple2K[Id, Id, *], A]
  type Tree[A]    = Cofree[TreeF, A]

  def leaf[A](a: A): Tree[A] = Cofree(a, Eval.now(OptionT.none))
  def branch[A](a: A, left: Tree[A], right: Tree[A]): Tree[A] =
    Cofree(a, Eval.now(OptionT.liftF(Tuple2K[Id, Id, Tree[A]](left, right))))

  val tree: Tree[Int] = branch(1, leaf(2), branch(3, leaf(4), leaf(5)))
  println(tree.map(_ + 2).toList)
}
