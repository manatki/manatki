package manatki.data

import cats.Eval.{later, now}
import cats.{Applicative, Comonad, Eval}
import tofu.syntax.monadic._

final case class Memo[+A](head: Eval[A], odds: Eval[Memo[A]], evens: Eval[Memo[A]]) extends (BigInt => Eval[A]) {
  def apply(index: BigInt): Eval[A] =
    if (index == Memo.zero) head
    else if (index.testBit(0)) odds.flatMap(_(index / 2))
    else evens.flatMap(_(index / 2 - 1))

  def zipMap[B, C](other: Memo[B])(f: (A, B) => C): Memo[C] =
    Memo((head, other.head).mapN(f), (odds, other.odds).mapN(_.zipMap(_)(f)), (evens, other.evens).mapN(_.zipMap(_)(f)))

}

object Memo {
  private val zero: BigInt = Numeric[BigInt].zero
  private def ini(top: BigInt): Memo[BigInt] =
    Memo(now(top), later(ini(top * 2 + 1)), later(ini(top * 2 + 2)))

  val naturals: Memo[BigInt] = ini(0)

  implicit object memoInstance extends Applicative[Memo] with Comonad[Memo] {
    def pure[A](x: A): Memo[A] = {
      lazy val res: Memo[A] = Memo(now(x), later(res), later(res))
      res
    }
    def extract[A](x: Memo[A]): A = x.head.value

    def coflatMap[A, B](fa: Memo[A])(f: Memo[A] => B): Memo[B] =
      Memo(Eval.later(f(fa)), fa.odds.map(coflatMap(_)(f)), fa.evens.map(coflatMap(_)(f)))

    def ap[A, B](ff: Memo[A => B])(fa: Memo[A]): Memo[B] = map2(ff, fa)(_(_))

    override def map2[A, B, Z](fa: Memo[A], fb: Memo[B])(f: (A, B) => Z): Memo[Z] = fa.zipMap(fb)(f)

    override def map[A, B](fa: Memo[A])(f: A => B): Memo[B] =
      Memo(fa.head.map(f), fa.odds.map(map(_)(f)), fa.evens.map(map(_)(f)))
  }
}
