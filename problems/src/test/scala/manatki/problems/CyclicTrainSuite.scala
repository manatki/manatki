package manatki.problems

import manatki.problems.CyclicTrain.{countWagons, cyclicTrainState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CyclicTrainSuite extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  "Solution" should "work for any input" in {
    forAll { (first: Boolean, rest: Vector[Boolean]) =>
      countWagons(cyclicTrainState).runA(first +: rest).value shouldBe rest.length + 1
    }
  }
}
