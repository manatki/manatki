import scala.annotation.tailrec

import cats.syntax.all._
import cats.{Applicative, Eval, Monad, Traverse}
sealed trait Tree[+A] {
  private def depthLz: Eval[Long] = this match {
    case Leaf(_)         => Eval.now(1)
    case Branch(_, l, r) => Eval.defer((l.depthLz, r.depthLz).mapN(_.max(_) + 1))
  }
  def depth                       = depthLz.value
}

case class Leaf[+A](value: A)                                  extends Tree[A]
case class Branch[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  implicit val treeFunctor: TreeMonad.type = TreeMonad
}

object TreeMonad extends Monad[Tree] with Traverse[Tree] {
  private def flatMapLz[A, B](ta: Tree[A])(f: A => Eval[Tree[B]], children: Children[B]): Eval[Tree[B]] = ta match {
    case Leaf(value)                => f(value)
    case Branch(value, left, right) =>
      val lch = Eval.defer(flatMapLz(left)(f, children)).memoize
      val rch = Eval.defer(flatMapLz(right)(f, children)).memoize
      f(value).flatMap(applyChildren(_, Some((lch, rch))))
  }

  type Children[B] = Option[(Eval[Tree[B]], Eval[Tree[B]])]

  private def applyChildren[B](tree: Tree[B], children: Children[B]): Eval[Tree[B]] =
    children match {
      case None           => Eval.now(tree)
      case Some((le, re)) =>
        val (tip, echildren) = tree match {
          case Leaf(tip)                => (tip, (le, re))
          case Branch(tip, left, right) =>
            (tip, (Eval.defer(applyChildren(left, children)), Eval.defer(applyChildren(right, children))))
        }
        echildren.mapN(Branch(tip, _, _))
    }

  private def tailRecLz[A, B](
      t: Tree[Either[A, B]],
      f: A => Tree[Either[A, B]],
      children: Children[B]
  ): Eval[Tree[B]] =
    t match {
      case Leaf(Right(b))          => applyChildren(Leaf(b), children)
      case Leaf(Left(a))           => Eval.defer(tailRecLz(f(a), f, children))
      case Branch(te, left, right) =>
        val le          = Eval.defer(tailRecLz(left, f, children)).memoize
        val re          = Eval.defer(tailRecLz(right, f, children)).memoize
        val newChildren = (le, re).some
        te match {
          case Left(value) => tailRecLz(f(value), f, newChildren)
          case Right(b)    => applyChildren(Leaf(b), newChildren)
        }
    }

  def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = flatMapLz(fa)(a => Eval.later(f(a)), None).value

  def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = tailRecLz(f(a), f, None).value

  def pure[A](x: A): Tree[A] = Leaf(x)

  def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = {
    @tailrec
    def go(cur: Tree[A], acc: B, stack: List[(A, Tree[A])]): B = cur match {
      case Leaf(value)                =>
        val res = f(acc, value)
        stack match {
          case (a, r) :: tail => go(r, f(res, a), tail)
          case Nil            => res
        }
      case Branch(value, left, right) => go(left, acc, (value, right) :: stack)
    }
    go(fa, b, Nil)
  }

  def foldRight[A, B](fa: Tree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
    case Leaf(value)                => f(value, lb)
    case Branch(value, left, right) =>
      Eval.defer(foldRight(fa, f(value, Eval.defer(foldRight(left, lb)(f))))(f))

  }

  def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = fa match {
    case Leaf(value)                => f(value).map(Leaf(_))
    case Branch(value, left, right) =>
      (traverse(left)(f), f(value), traverse(right)(f)).mapN((v, l, r) => Branch(l, v, r))
  }
}

object Main {
  val longTree: Tree[Long] =
    100000.some.tailRecM {
      case None    => 1L.asRight.pure[Tree]
      case Some(0) => none.asLeft.pure[Tree]
      case Some(i) => Branch(i.toLong.asRight, i.toLong.asRight.pure[Tree], (i - 1).some.asLeft.pure[Tree])
    }

  def main(args: Array[String]) = {
    println(longTree.toList)
    println(longTree.depth)
    println(longTree.size)
    println(longTree.sumAll)
  }
}
