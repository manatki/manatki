package manatki.data.tagless
import manatki.data.tagless.data.ListP
import tofu.syntax.monadic._
import ProCorep.ops._

class ListPSuite extends munit.FunSuite {

  val largeList = ListP(1L to 100_000L: _*)
  val sum       = new ListP[Long, Long, Long] {
    def nil                    = 0
    def cons(a: Long, y: Long) = a + y
  }

  val count = new ListP[Any, Long, Long] {
    def nil                         = 0
    def cons(a: Any, y: Long): Long = y + 1
  }

  def toList[A] = new ListP[A, List[A], List[A]] {
    def nil                    = Nil
    def cons(a: A, y: List[A]) = a :: y
  }

  type IntList[-A, +B] = ListP[Long, A, B]
  val lst       = ProCorep.construct[IntList]
  val emptyList = Layer[IntList](_.nil)
  val smallList = lst.cons(1, lst.cons(2, lst.nil))
  test("small sum") {
    assertEquals(smallList.fold(sum), 3L)
  }

  test("large sum") {
    assertEquals(largeList.foldS(sum), 5_000_050_000L)
  }

  test("small to list") {
    assertEquals(smallList.foldS(toList), List(1L, 2L))
  }

  test("large map and zip") {
    assertEquals(largeList.map(_ + 1).foldS(sum.zip(count)), (5_000_150_000L, 100_000L))
  }
}
