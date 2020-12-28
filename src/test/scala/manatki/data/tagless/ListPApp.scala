package manatki.data.tagless
import manatki.data.tagless.data.ListP
import tofu.syntax.monadic._
import ProCorep.ops._

class ListPSuite extends munit.FunSuite {

  val largeList = ListP(1L to 100_000L: _*)
  val ops = ListP.foldOps[Long]

  type IntList[-A, +B] = ListP[Long, A, B]
  val lst       = ProCorep.construct[IntList]
  val emptyList = Layer[IntList](_.nil)
  val smallList = lst.cons(1, lst.cons(2, lst.nil))
  test("small sum") {
    assertEquals(smallList.sum, 3L)
  }

  test("large sum") {
    assertEquals(largeList.sum, 5_000_050_000L)
  }

  test("small to list") {
    assertEquals(smallList.toList, List(1L, 2L))
  }

  test("large map and zip") {
    assertEquals(largeList.map(_ + 1).foldS(ops.sum.zip(ops.length)), (5_000_150_000L, 100_000L))
  }
}
