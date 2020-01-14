package manatki.data.tagless
import manatki.data.tagless.data.ListP
import tofu.syntax.monadic._
import ProCorep.ops._

object ListPApp extends App {

  val largeList = ListP(1L to 100000L: _*)
  val sum = new ListP[Long, Long, Long] {
    def nil                    = 0
    def cons(a: Long, y: Long) = a + y
  }
  def toList[A] = new ListP[A, List[A], List[A]] {
    def nil                    = Nil
    def cons(a: A, y: List[A]) = a :: y
  }

//  println(largeList.foldL(sum).value)
//  println(largeList.foldL(toList).value)
  println(largeList.map(_ + 1).foldL(sum.zip(toList[Long])).value)
}
