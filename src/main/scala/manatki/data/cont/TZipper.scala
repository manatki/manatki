package manatki.data.cont

import cats.Eval.now
import cats.effect.{IO, Sync}
import cats.instances.int._
import cats.instances.sortedMap._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats._
import Cont.reset


/** Kiselyov generic traversable Zipper */
sealed trait TZipper[T[_], A] {
  import TZipper._
  def zipUp: T[A] = this match {
    case Done(result)         => result
    case Cursor(_, transform) => transform(None).zipUp
  }
}

object TZipper {
  def apply[T[_]: Traverse, A](fa: T[A]): TZipper[T, A] = {
    type CT[X] = Cont[TZipper[T, A], X]
    reset(fa.traverse[CT, A](a => Cont.shift(k => Cursor[T, A](a, oa => k(now(oa.getOrElse(a))).value).pure[CT])).map(Done(_))).value
  }
  type Move[T[_], A] = Option[A] => TZipper[T, A]
  final case class Done[T[_], A](result: T[A])                   extends TZipper[T, A]
  final case class Cursor[T[_], A](current: A, move: Move[T, A]) extends TZipper[T, A]
}

