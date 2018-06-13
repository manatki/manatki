package manatki.data.cont

import cats.Eval.later
import cats.Monoid
import cats.instances.function._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest.FlatSpec

class ContSuite extends FlatSpec {
  val simpleList = for {
    x <- List.range(1, 6)
    y <- List.range(1, x) if (x + y) % 3 == 0
    _ <- if ((x + y) % 3 == 0) List(()) else List.empty
  } yield (x, y)

  def listEffect[R: Monoid](size: Int, mod: Int): Cont[R, (Int, Int)] = for {
    x <- Cont.range[R](1, size)
    y <- Cont.range[R](1, x) if (x + y) % mod == 0
  } yield (x, y)

  def listReader[R: Monoid](size: Int): Cont[Int => R, (Int, Int)] = for {
    mod <- Cont.get[Int, R]
    res <- listEffect[Int => R](6, mod)
  } yield res

  val sumsSum = List.range(1L, 10001L).scan(0L)(_ + _).sum

  "list effect" should "be equivalent to straightforward" in
    assert(listEffect[List[(Int, Int)]](6, 3).runS(p => List(p)) === simpleList)

  "reader list" should "be equivalent to straghtforward" in
   assert(listReader[List[(Int, Int)]](6).runS(p => _ => List(p))(3) === simpleList)

  "scan via state" should "yield correct result withoud stack overflow" in
    assert(List.range(1L, 10001L)
      .traverse[Cont.State[List[Long], Long, ?], Long](x => Cont.state(i => (i + x, i + x)))
      .runS(l => i => later((l, i)))(0L).value._1.sum === sumsSum)
}
