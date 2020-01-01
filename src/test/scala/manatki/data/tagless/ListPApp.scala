package manatki.data.tagless
import manatki.data.tagless.data.ListP

object ListPApp extends App {
  println(ListP(1 to 10000: _*).foldL(new ListP[Int, Int, Int] {
    def nil: Int                  = 0
    def cons(a: Int, y: Int): Int = a + y
  }).value)
}
