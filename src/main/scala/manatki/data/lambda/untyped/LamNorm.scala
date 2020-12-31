package manatki.data.lambda.untyped

import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.{Applicative, Eval}
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless._
import tofu.higherKind.derived.representableK
import tofu.syntax.monadic._

trait LamNorm[-I, +O] {
  def varApp(vari: String, args: Vector[I]): O
  def lam(param: String, body: I): O

  final def vapp(name: String, args: I*) = varApp(name, args.toVector)
}

object LamNorm extends ProfData[LamNorm] with ProfFoldOps with ProfMatchOps {

  object nat {
    import mk._
    def zero        = lam("s", lam("z", vapp("z")))
    def one         = lam("s", lam("z", vapp("s", vapp("z"))))
    def int(x: Int) = lam("s", lam("z", (1 to x).foldLeft(vapp("z"))((t, _) => vapp("s", t))))
  }

  object foldOps extends FoldOps[Fold] {
    object lam extends Fold[Lam.T] {
      def varApp(name: String, args: Vector[Lam.T]): Lam.T =
        args.foldLeft(Lam.mk.vari(name))((f, a) => Lam.mk.app(f, a))
      def lam(param: String, body: Lam.T): Lam.T           = Lam.mk.lam(param, body)
    }
    val show = showFold
  }

  implicit val proTraverse: ProTraverse[LamNorm] = new ProTraverse[LamNorm] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): LamNorm[A, C] =
      new Tab(left, right) with ProTab[F, A, B, C, LamNorm]
  }

  abstract class FoldOps[F[_]] {
    def lam: F[Lam.T]
    def show: F[String]
  }

  implicit lazy val foldFunctor = representableK.instance

  trait ProTab[F[_], A, B, C, P[-x, +y] <: LamNorm[x, y]] extends Tab[F, A, B, C, P] with LamNorm[A, C] {
    def varApp(vari: String, args: Vector[A]): C =
      right(args.traverse(left).map(bs => rep(_.varApp(vari, bs))))

    def lam(param: String, body: A): C = right(left(body).map(b => rep(_.lam(param, b))))
  }

  implicit lazy val showFold: Fold[String] = new Fold[String] {
    def varApp(name: String, args: Vector[String]): String =
      if (args.isEmpty) name else args.mkString(name + " (", ") (", ")")

    def lam(param: String, body: String): String = s"λ $param.$body"
  }

  abstract class MatchOps[F[_]] {
    def rename(old: String, neu: String): F[T]
    def isFree(name: String): F[Boolean]
    def substitute(name: String, expr: T): F[Eval[T]]
    def apply(expr: T): F[Eval[T]]
  }

  lazy val matchFunctor = representableK.instance

  object matchOps extends MatchOps[Match] {
    def rename(old: String, neu: String) = new Match[T] {
      def varApp(name: String, args: Vector[T]): T =
        mk.varApp(if (name == old) neu else name, args.map(_.unpack(this)))
      def lam(param: String, body: T): T           =
        mk.lam(param, if (param == old) body else body.unpack(this))
    }

    def isFree(s: String) = new Match[Boolean] {
      def varApp(name: String, args: Vector[T]): Boolean = name == s || args.exists(_.unpack(this))
      def lam(param: String, body: T): Boolean           = !(param == s) && body.unpack(this)
    }

    def substitute(name: String, expr: T) = new Match[Eval[T]] {
      def varApp(vari: String, args: Vector[T]): Eval[T] =
        for {
          nargs <- args.traverse(_.unpack(this))
          res   <- if (vari == name) nargs.foldLeftM(expr)(_ apply _)
                   else Eval.now(mk.varApp(vari, nargs))
        } yield res

      def lam(param: String, body: T): Eval[T] = if (expr.isFree(param)) {
        val newName = Iterator.from(1).map(i => s"$param$i").find(x => !body.isFree(x) && !expr.isFree(x)).get
        body.rename(param, newName).substitute(name, expr).map(mk.lam(newName, _))
      } else if (param == name) Eval.now(mk.lam(param, body))
      else body.substitute(name, expr).map(mk.lam(param, _))
    }

    def apply(expr: T) = new Match[Eval[T]] {
      def varApp(vari: String, args: Vector[T]): Eval[T] = Eval.now(mk.varApp(vari, args :+ expr))
      def lam(param: String, body: T): Eval[T]           = body.substitute(param, expr)
    }
  }
}
