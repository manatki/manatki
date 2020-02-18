package manatki

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FsFsSuite extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {
  "fsfs.zipWithHeader" should {
    "zip with non-empty header" when {
      "stream is non-empty" in forAll {
        (x: Int, xs: Vector[Int]) =>
          fs2.Stream(x +: xs: _*)
            .through(fsfs.zipWithHeader)
            .toVector should be(xs.map((x, _)))
      }
      "stream is empty" in {
        fs2.Stream.empty.through(fsfs.zipWithHeader).toVector should be(Vector.empty)
      }
    }
  }
}
