package manatki.data.lambda.untyped

import cats.Applicative
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless._
import tofu.higherKind.derived.representableK

trait Lam[-I, +O] {
  def vari(name: String): O
  def app(f: I, arg: I): O
  def lam(param: String, body: I): O
}

object Lam extends ProfData[Lam] with ProfMatchOps with ProfFoldOps {
  implicit class LamOps(private val x: T) extends AnyVal {
    def apply(ys: T*): T = ys.foldLeft(x)(mk.app)
    def show             = x.showX._1
  }

  implicit class stringOps(private val name: String)         extends AnyVal {
    def lam(body: T): T                 = mk.lam(name, body)
    def lam(names: String*)(body: T): T = (name +: names).foldRight(body)(mk.lam)
    def v: T                            = mk.vari(name)
    def $ : T                           = mk.vari(name)
  }
  implicit class stringCtxOp(private val ctx: StringContext) extends AnyVal {
    def lc(lams: T*) = LamParser(ctx.s(lams.map(_.show).map(s => s"($s)"): _*))
  }

  object nat {
    def int(x: Int) = "s".lam("z")((1 to x).foldLeft("z".v)((t, _) => "s".v(t)))
    val zero        = lc"^s z.z"
    val one         = lc"^s z.s z"
    val plus        = lc"^a b s z.a s (b s z)"
    val mul         = lc"^x y.x ($plus y) $zero"
    val pow         = lc"^x y.y ($mul x ) $one"
    val mul_        = lc"^x y s.x (y s)"
    val pow_        = lc"^x y s.y x s"
    val pow__       = lc"^x y.y x"
    val pair        = lc"^x y f.f x y"
    val fst         = lc"^p.p(^f s.f)"
    val snd         = lc"^p.p(^f s.s)"
    val pred        = lc"^x s z.$fst(x (^p.$pair ($snd p) (s ($snd p))) ($pair z z))"
    val minus       = lc"^x y.y $pred x"
  }

  val showFold = new Fold[(String, Int)] {
    def vari(name: String) = (name, 1)

    private def wrap(x: (String, Int), pos: Int)  = if (x._2 > pos) s"(${x._1})" else x._1
    def app(f: (String, Int), arg: (String, Int)) = (s"${wrap(f, 3)} ${wrap(arg, 2)}", 3)

    def lam(param: String, body: (String, Int)) = (s"λ $param.${body._1}", 4)
  }

  val proTraverse = new ProTraverse[Lam] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): Lam[A, C] =
      new Tab(left, right) with ProTab[F, A, B, C, Lam]
  }

  trait ProTab[F[_], A, B, C, P[-x, +y] <: Lam[x, y]] extends Tab[F, A, B, C, P] with Lam[A, C] {
    def vari(name: String): C          = mkPure(_.vari(name))
    def app(f: A, arg: A): C           = mkMap2(f, arg)((fb, argb) => _.app(fb, argb))
    def lam(param: String, body: A): C = mkMap(body)(b => _.lam(param, b))
  }
  type NormPrep[-I, +O] = Lam[I, (Long, Set[String]) => O]
  abstract class MatchOps[F[_]] {
    def isFree(name: String): F[Boolean]
  }

  lazy val matchFunctor = representableK.instance
  object matchOps extends MatchOps[Match] {
    def isFree(varName: String) = new Match[Boolean] {
      def vari(name: String): Boolean = name == varName

      def app(f: T, arg: T): Boolean = f.isFree(varName) || arg.isFree(varName)

      def lam(param: String, body: T): Boolean = !(param == varName) && body.isFree(varName)
    }
  }

  type DBJ = (Int, Map[String, Int]) => debrujin.Lam.T
  abstract class FoldOps[F[_]] {
    def showX: F[(String, Int)]
    def norm: F[LamNorm.T]
    def dbj: F[DBJ]
  }
  lazy val foldFunctor = representableK.instance

  object foldOps extends FoldOps[Fold] {
    object showX extends Fold[(String, Int)] {
      def vari(name: String) = (name, 1)

      private def wrap(x: (String, Int), pos: Int)  = if (x._2 > pos) s"(${x._1})" else x._1
      def app(f: (String, Int), arg: (String, Int)) = (s"${wrap(f, 3)} ${wrap(arg, 2)}", 3)

      def lam(param: String, body: (String, Int)) = (s"λ $param.${body._1}", 4)
    }
    object norm  extends Fold[LamNorm.T]     {
      def vari(name: String): LamNorm.T                  = LamNorm.mk.vapp(name)
      def lam(param: String, body: LamNorm.T): LamNorm.T = LamNorm.mk.lam(param, body)
      def app(f: LamNorm.T, arg: LamNorm.T): LamNorm.T   = f.apply(arg).value
    }

    object dbj extends Fold[DBJ] {
      private[this] val dbj       = debrujin.Lam.mk
      def vari(name: String): DBJ = (_, m) => dbj.vari(m(name))

      def app(f: DBJ, arg: DBJ): DBJ = (d, m) => dbj.app(f(d, m), arg(d, m))

      def lam(param: String, body: DBJ): DBJ = (d, m) => dbj.lam(param, body(d + 1, m.updated(param, d)))
    }
  }
}

object LamParser {
  import fastparse._
  import fastparse.ScalaWhitespace._

  def name[_: P]        = P(CharIn("a-z", "A-Z") ~~ CharIn("a-z", "A-Z", "_'", "0-9").repX).!
  def lambda[_: P]      =
    P(CharIn("^λ") ~ name.rep(1) ~ "." ~ term)
      .map { case (names, body) => names.foldRight(body)(Lam.mk.lam) }
  def variable[_: P]    = name.map(Lam.mk.vari)
  def paren[_: P]       = P("(" ~ term ~ ")")
  def simple[_: P]      = P(variable | paren)
  def application[_: P] = simple.rep(1).map(_.reduce(Lam.mk.app))

  def term[_: P]: P[Lam.T] = P(application | lambda)
  def wholeTerm[_: P]      = term ~ End

  def apply(s: String) = parse(s, wholeTerm(_)).get.value
}
