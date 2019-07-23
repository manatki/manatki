package manatki.problems

import manatki.problems.CyclicTrain.{countWagons, cyclicTrainState}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class CyclicTrainSuite extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "Solution" should "work for any input" in {
    forAll { (first: Boolean, rest: Vector[Boolean]) =>
      countWagons(cyclicTrainState).runA(first +: rest).value shouldBe rest.length + 1
    }
  }
}
