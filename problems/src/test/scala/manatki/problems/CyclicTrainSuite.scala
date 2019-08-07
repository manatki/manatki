package manatki.problems

import manatki.problems.CyclicTrain.{countWagons, cyclicTrainState}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CyclicTrainSuite extends FlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  "Solution" should "work for any input" in {
    forAll { (first: Boolean, rest: Vector[Boolean]) =>
      countWagons(cyclicTrainState).runA(first +: rest).value shouldBe rest.length + 1
    }
  }
}
