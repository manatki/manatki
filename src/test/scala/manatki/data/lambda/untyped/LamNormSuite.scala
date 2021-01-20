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
  import Lam.nat._
  import Lam.stringOps
  import Normalize.{encoder, jsoner}
  import io.circe.syntax._

  val exp1 = "x".v("x".lam("x".v), "y".lam("x".v))
  println(Normalize.prepare(exp1).asJson.spaces2)
  println(Normalize.prepare(exp1).unpack(Normalize.renamer("x", "z")).asJson.spaces2)
  println(Normalize(plus(int(2), int(2))).show)
  println(Normalize(mul(mul(int(2), int(2)), int(2))).show)
  println(Normalize(pow(int(2), int(2))).show)
  println(Normalize(pow(int(3), int(2))).show)
  println(Normalize(pow(int(2), int(3))).show)
  println(Normalize(mul_(int(2), int(3))).show)
  println(Normalize(mul_(int(3), int(2))).show)
  println(Normalize(pow_(int(3), int(2))).show)
  println(Normalize(pow_(int(2), int(3))).show)
  println(Normalize(pow__(int(2), int(3))).show)
//  println(Normalize.nf(plus(int(2), int(2))).va.show)
}

object LamNormCheck extends App {
  import Lam.nat._
  import Lam.stringOps
  import Normalize.{encoder, jsoner}
  import io.circe.syntax._

  val exp1              = "x".v("x".lam("x".v), "y".lam("x".v))
  def check(exp: Lam.T) = println(exp.norm.show)
  check(int(0))
  check(int(2))
  check(exp1)
  check(plus(int(2), int(2)))
  check(mul(int(1), int(0)))
  check(mul(int(3), int(3)))
  check(mul_(int(3), int(3)))
  check(mul(mul(int(2), int(2)), int(2)))
  check(pow(int(2), int(2)))
  check(pow(int(2), int(3)))
  check(pow_(int(2), int(3)))
  check(pow_(int(3), int(2)))
  check(pow__(int(2), int(3)))
  check(pred(int(4)))
  check(minus(int(7), int(5)))
  check(pow(int(5), int(2)))
  check(mul(int(4), int(6)))
  check(minus(pow(int(3), int(2)), mul(int(2), int(3))))
}
