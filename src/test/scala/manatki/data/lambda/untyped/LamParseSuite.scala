package manatki.data.lambda.untyped

import munit.FunSuite

class LamParseSuite extends FunSuite {
  import Lam._
  test("parsing") {
    assertEquals(lc"s".show, "s")
    assertEquals(lc"s z".show, "s z")
    assertEquals(lc"^s z.s z".show, "λ s.λ z.s z")
  }
}