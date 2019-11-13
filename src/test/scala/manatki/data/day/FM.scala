package manatki.data.day

import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}
class FM extends FlatSpec with Matchers {

  "FreeMonoidal" should "traverse large list" in {
    List
      .range(1, 1000)
      .traverse(i => FreeMonoidal.lift[(Int, *), Int]((i, i)))
      .fold shouldBe ((1 until 1000).sum, (1 until 1000).toList)
  }

}
