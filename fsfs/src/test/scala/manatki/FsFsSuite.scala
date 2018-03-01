package manatki

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

import scala.collection.immutable

class FsFsSuite extends WordSpec with PropertyChecks with Matchers {
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
