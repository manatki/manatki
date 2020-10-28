package manatki.data.simple

object GoodScala {
  sealed trait List[+A]
  sealed trait OrList[+A, +B] extends List[B]
  sealed trait Option[+A]     extends List[A]
  sealed trait Empty          extends Option[Nothing]
  sealed trait NonEmpty[+A]   extends List[A] {
    def value: A
    def rest: List[A]
  }
  sealed trait Either[+L, +R] extends Option[R] with OrList[L, R]

  final case class Left[+L](value: L)                    extends Either[L, Nothing] with Empty
  final case class Right[+R](value: R)                   extends Either[Nothing, R] with NonEmpty[R] {
    def rest = nil
  }
  final case class Cons[+A](value: A, rest: NonEmpty[A]) extends OrList[Nothing, A] with NonEmpty[A]

  val nil: Option[Nothing] = Left(())
  object Empty {
    def unapply(x: Left[Any]): Boolean = true
  }
}
