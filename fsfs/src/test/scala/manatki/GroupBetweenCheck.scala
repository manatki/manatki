package manatki
import fs2._
import manatki.fsfs._
import org.scalatest.funsuite.AnyFunSuite
import Ordering.Implicits._

class GroupBetweenCheck extends AnyFunSuite {
  test("group between with") {
    val stream: Stream[Pure, (String, String)] = Stream(
      ("start" -> "false"),
      ("B"     -> "150"),
      ("B"     -> "150"),
      ("B"     -> "150"),
      ("A"     -> "150"),
      ("A"     -> "150"),
      ("start" -> "false"),
      ("B"     -> "15"),
      ("A"     -> "15"),
      ("start" -> "true"),
      ("A"     -> "1"),
      ("B"     -> "1"),
      ("start" -> "true"),
      ("B"     -> "3"),
      ("A"     -> "3"),
      ("B"     -> "2"),
      ("A"     -> "2"),
      ("start" -> "true"),
      ("B"     -> "1"),
      ("B"     -> "1"),
      ("B"     -> "2"),
      ("A"     -> "1"),
      ("start" -> "false"),
    )

    val result = stream.groupBetweenWith {
      case ("start", "true")  => true
      case ("start", "false") => false
    }(identity)(Ordering[String].max)

    assert(result.toList === List(Map("A" -> "3", "B" -> "3")))
  }

}
