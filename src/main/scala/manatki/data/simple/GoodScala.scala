package manatki.data.simple

import scala.annotation.nowarn

object GoodScala {
  sealed trait List[+A] {
    def isEmpty: Boolean
    def ::[B >: A](b: B): List[B]

    @nowarn
    def foldLeft[B](b: B)(f: (B, A) => B): B = {
      def go(b: B, l: List[A]): B = l match {
        case Empty()   => b
        case x :: rest => go(f(b, x), rest)
      }
      go(b, this)
    }
  }
  sealed trait OrList[+A, +B] extends List[B]
  sealed trait Option[+A]     extends List[A]         {
    def get: A
  }
  sealed trait Empty          extends Option[Nothing] {
    def isEmpty     = true
    def get         = throw new NoSuchElementException
    def ::[B](b: B) = Right(b)
  }
  sealed trait NonEmpty[+A]   extends List[A]         {
    def value: A
    def rest: List[A]
    def isEmpty          = false
    def ::[B >: A](b: B) = Cons(b, this)
  }
  sealed trait Either[+L, +R] extends Option[R] with OrList[L, R]

  final case class Left[+L](value: L)                    extends Either[L, Nothing] with Empty
  final case class Right[+R](value: R)                   extends Either[Nothing, R] with NonEmpty[R] {
    def rest = nil
    def get  = value
  }
  final case class Cons[+A](value: A, rest: NonEmpty[A]) extends OrList[Nothing, A] with NonEmpty[A]

  val nil: Option[Nothing] = Left(())
  object Empty {
    def unapply(x: Left[Any]): Boolean = true
  }
  object ::    {
    def unapply[A](xs: List[A]): Option[(A, List[A])] = xs match {
      case e: Empty       => e
      case n: NonEmpty[A] => Right((n.value, n.rest))
    }
  }
}
