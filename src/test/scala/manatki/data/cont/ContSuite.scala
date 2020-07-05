package manatki.data.cont

import cats.Eval.{later, now}
import cats.{Eval, Foldable, Monoid}
import cats.instances.function._
import cats.instances.list._
import cats.instances.lazyList._
import cats.instances.int._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.monoid._
import cats.syntax.foldable._
import ContE.{reset, resetS, shift, shiftS}
import org.scalatest.FlatSpec
import Monoid.empty
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Second, Span}
import org.scalatest.flatspec.AnyFlatSpec

class ContSuite extends AnyFlatSpec with TimeLimitedTests {

  val timeLimit = Span(1, Second)

  val simpleList = for {
    x <- List.range(1, 6)
    y <- List.range(1, x) if (x + y) % 3 == 0
    _ <- if ((x + y) % 3 == 0) List(()) else List.empty
  } yield (x, y)

  def listEffect[R: Monoid](size: Int, mod: Int): ContE[R, (Int, Int)] =
    for {
      x <- ContE.range[R](1, size)
      y <- ContE.range[R](1, x) if (x + y) % mod == 0
    } yield (x, y)

  def listReader[R: Monoid](size: Int): ContE[Int => Eval[R], (Int, Int)] =
    for {
      mod <- ContE.get[Int, R]
      res <- listEffect[Int => Eval[R]](6, mod)
    } yield res

  val sumsSum = List.range(1L, 10001L).scan(0L)(_ + _).sum

  "list effect" should "be equivalent to straightforward" in
    assert(listEffect[List[(Int, Int)]](6, 3).runS(p => List(p)) === simpleList)

  "reader list" should "be equivalent to straghtforward" in
    assert(listReader[List[(Int, Int)]](6).runS(p => _ => now(List(p)))(3).value === simpleList)

  "scan via state" should "yield correct result withoud stack overflow" in
    assert(
      List
        .range(1L, 10001L)
        .traverse[ContE.State[List[Long], Long, *], Long](x => ContE.state(i => (i + x, i + x)))
        .runS(l => i => now(l))(0L)
        .value
        .sum === sumsSum)

}
