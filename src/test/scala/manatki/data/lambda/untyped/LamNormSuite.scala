package manatki.data.lambda.untyped
import cats.syntax.show._
import munit.FunSuite

class LamNormSuite extends FunSuite {
  test("lam show") {
    import LamNorm.mk._

    assertEquals(lam("a", lam("b", vapp("b", vapp("a")))).show, "λ a.λ b.b (a)")
    assertEquals(lam("s", lam("z", vapp("s", vapp("s", vapp("z"))))).show, "λ s.λ z.s (s (z))")
    assertEquals(LamNorm.nat.int(3).show, "λ s.λ z.s (s (s (z)))")
  }

//  test("some norm") {
//    import Lam.LamOps
//    import Lam.nat._
//    assertEquals(mul(int(1), int(0)).normal.show, int(0).normal.show)
//  }
//
//  test("lam normalize") {
//    import Lam.nat._
//    import Lam.{LamOps, stringOps}
//
//    assertEquals(plus(int(2), int(2)).normal.show, "λ s.λ z.s (s (s (s (z))))")
//    assertEquals(plus(int(3), int(4)).normal.show, "λ s.λ z.s (s (s (s (s (s (s (z)))))))")
//    assertEquals(zero.normal.show, "λ s.λ z.z")
//    assertEquals(int(4).normal.show, "λ s.λ z.s (s (s (s (z))))")
//    val ident = "x".lam("x".vari)
//    val S     = "x".lam("y")("x".vari)
//    assertEquals(ident(ident).normal.show, "λ x.x")
//    assertEquals(ident.normal.show, "λ x.x")
//    assertEquals(ident(ident(ident)).normal.show, "λ x.x")
//    assertEquals("x".lam("x".lam("x".vari))(zero).normal.show, "λ x.x")
////    assertEquals(int(0)(plus(int(2))).normal.show, "λ s.λ z.s (s (s (s (z))))")
//    assertEquals(mul(int(1), int(0)).normal.show, int(0).normal.show)
////    assertEquals(plus(int(3), int(4)).normal.show, "λ s.λ z.s (s (s (s (s (s (s (z)))))))")
//  }
}

object LamPrepCheck extends App {
  import Lam.mk._
  import Lam.nat._
  import Lam.stringOps
  import io.circe.syntax._

  import Normalize.encoder
  import Normalize.jsoner

  val exp1 = "x".vari("x".lam("x".vari), "y".lam("x".vari))
  println(Normalize.prepare(exp1).asJson.spaces2)
  println(Normalize.prepare(exp1).unpack(Normalize.rename("x", "z")).asJson.spaces2)
  println(Normalize(plus(int(2), int(2))).show)
  println(Normalize(mul(mul(int(2), int(2)), int(2))).show)
  println(Normalize(pow(int(2), int(2))).show)
//  println(Normalize.nf(plus(int(2), int(2))).va.show)
}
