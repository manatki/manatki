package manatki.data.lambda.untyped

import cats.Applicative
import manatki.data.lambda
import manatki.data.lambda.untyped
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless._
import tofu.higherKind.derived.representableK
import tofu.syntax.monadic._

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

  implicit class stringOps(private val name: String) extends AnyVal {
    def lam(body: T): T                 = mk.lam(name, body)
    def lam(names: String*)(body: T): T = (name +: names).foldRight(body)(mk.lam)
    def vari: T                         = mk.vari(name)
  }

  object nat {
    def int(x: Int) = "s".lam("z")((1 to x).foldLeft("z".vari)((t, _) => "s".vari(t)))
    val zero        = int(0)
    val one         = int(1)
    def plus        = "a".lam("b", "s", "z")("a".vari("s".vari, "b".vari("s".vari, "z".vari)))
    def mul         = "x".lam("y")("x".vari(plus("y".vari), zero))
    def pow         = "x".lam("y")("y".vari(mul("x".vari), one))
    def mul_        = "x".lam("y", "s")("x".vari("y".vari("s".vari)))
    def pow_        = "x".lam("y", "s")("y".vari("x".vari)("s".vari))
    def pow__       = "x".lam("y")("y".vari("x".vari))

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
    def vari(name: String): C          = right(rep(_.vari(name)).pure)
    def app(f: A, arg: A): C           = right(left(f).map2(left(arg))((fb, argb) => rep(_.app(fb, argb))))
    def lam(param: String, body: A): C = right(left(body).map(b => rep(_.lam(param, b))))
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

  abstract class FoldOps[F[_]] {
    def showX: F[(String, Int)]
    def norm: F[LamNorm.T]
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
  }
}
