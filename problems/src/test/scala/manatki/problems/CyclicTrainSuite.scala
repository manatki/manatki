package manatki.problems

import cats.data.State
import manatki.problems.CyclicTrain.{CT, countWagons, cyclicTrainState}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.collection.immutable.BitSet

class CyclicTrainSuite extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "Solution" should "work for any input" in {
    forAll { (first: Boolean, rest: Vector[Boolean]) =>
      countWagons(cyclicTrainState).runA(first +: rest).value shouldBe rest.length + 1
    }
  }
}
