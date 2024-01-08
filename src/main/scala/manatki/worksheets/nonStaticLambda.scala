package manatki.worksheets

object nonStaticLambda {
  private def x: Int => String = (i: Int) => i.toString

  def main(args: Array[String]) = {
    println(x.getClass)
    println(x == x) // true on Scala 2, false on Scala 3...
    println(x.hashCode -> x.hashCode) // same on Scala 2, different on Scala 3
  }

}