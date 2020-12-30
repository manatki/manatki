package manatki.data.lambda.untyped

import cats.syntax.traverse._
import cats.{Applicative, Eval}
import io.circe.Json
import manatki.data.lambda
import manatki.data.lambda.untyped.Lam.NormPrep
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless._
import tofu.data.ICalcM
import tofu.data.calc.CalcM
import tofu.higherKind.derived.representableK
import tofu.syntax.monadic._

trait LamNorm[-I, +O] {
  def varApp(name: String, args: Vector[I]): O
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

  implicit val proTraverse = new ProTraverse[LamNorm] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): LamNorm[A, C] =
      new Tab(left, right) with ProTab[F, A, B, C, LamNorm]
  }

  abstract class FoldOps[F[_]] {
    def lam: F[Lam.T]
    def show: F[String]
  }

  implicit lazy val foldFunctor = representableK.instance

  trait ProTab[F[_], A, B, C, P[-x, +y] <: LamNorm[x, y]] extends Tab[F, A, B, C, P] with LamNorm[A, C] {
    def varApp(name: String, args: Vector[A]): C =
      right(args.traverse(left).map(bs => rep(_.varApp(name, bs))))

    def lam(param: String, body: A): C = right(left(body).map(b => rep(_.lam(param, b))))
  }

  implicit lazy val showFold: Fold[String] = new Fold[String] {
    def varApp(name: String, args: Vector[String]): String =
      if (args.isEmpty) name else args.mkString(name + " (", ") (", ")")

    def lam(param: String, body: String): String = s"λ $param.$body"
  }

  abstract class MatchOps[F[_]] {
    def rename(old: String, neu: String): F[T]
  }

  lazy val matchFunctor = representableK.instance

  object matchOps extends MatchOps[Match] {
    def rename(old: String, neu: String) = new Match[T] {
      def varApp(name: String, args: Vector[T]): T =
        mk.varApp(if (name == old) neu else name, args.map(_.unpack(this)))
      def lam(param: String, body: T): T           =
        mk.lam(param, if (param == old) body else body.unpack(this))
    }
  }
}

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
  }
  lazy val foldFunctor = representableK.instance

  object foldOps extends FoldOps[Fold] {
    object showX extends Fold[(String, Int)] {
      def vari(name: String) = (name, 1)

      private def wrap(x: (String, Int), pos: Int)  = if (x._2 > pos) s"(${x._1})" else x._1
      def app(f: (String, Int), arg: (String, Int)) = (s"${wrap(f, 3)} ${wrap(arg, 2)}", 3)

      def lam(param: String, body: (String, Int)) = (s"λ $param.${body._1}", 4)
    }
  }
}

object Normalize extends ProfData[NormPrep] {
  type NP = Layer[NormPrep]
  val NP = mk
  type Go[+A] = ICalcM[Nothing, Any, Long, Nothing, A]

  lazy val proTraverse: ProTraverse[NormPrep] = implicitly

  private val increment: Go[Long] = CalcM.update(_ + 1)

  private val runner = new Lam[(NP, Set[String]), Go[(NP, Set[String])]] {
    def vari(name: String): Go[(NP, Set[String])]                                =
      for (i <- increment; s = Set(name)) yield NP.vari(name)(i, s) -> s

    def app(f: (NP, Set[String]), arg: (NP, Set[String])): Go[(NP, Set[String])] =
      for (i <- increment; s = f._2 union arg._2) yield NP.app(f._1, arg._1)(i, s) -> s

    def lam(param: String, body: (NP, Set[String])): Go[(NP, Set[String])]       =
      for (i <- increment; s = body._2 - param) yield NP.lam(param, body._1)(i, s) -> s
  }

  def prepare(lam: lambda.untyped.Lam.T): NP = lam.foldM(runner).runSuccessUnit(0)._2._1

  def prepared(np: NP): (Long, Set[String]) =
    np.unpack(Lam.cotabulate[NP](_ => (_, _)))

  import io.circe.syntax._

  implicit val jsoner = new Fold[Json] {
    def vari(name: String) = (i, s) => Vector(s"var $name [$i, $s]").asJson

    def app(f: Json, arg: Json) = (i, s) => Vector(s"app [$i, $s]".asJson, f, arg).asJson

    def lam(param: String, body: Json) = (i, s) => Vector(s"lam $param [$i, $s]".asJson, body).asJson
  }

  type HNF = Rep[LamNorm, (NP, Context)]

  private def runHNF(ctx: Context): Lam[NP, Eval[HNF]] = new Lam[NP, Eval[HNF]] {
    final def vari(name: String): Eval[HNF] = ctx.bindings.get(name) match {
      case None            => Eval.now(Rep[LamNorm, (NP, Context)](_.vapp(name)))
      case Some((np, ctx)) => hnf(np, ctx)
    }

    def app(f: NP, arg: NP): Eval[HNF] =
      hnf(f, ctx).flatMap(
        _(
          new LamNorm[(NP, Context), Eval[HNF]] {
            def varApp(name: String, args: Vector[(NP, Context)]): Eval[HNF] =
              Eval.now(Rep[LamNorm, (NP, Context)](_.varApp(name, args :+ (arg, ctx))))

            def lam(param: String, bc: (NP, Context)): Eval[HNF] = {
              val (i, free)      = prepared(arg)
              val (body, bodCtx) = bc
              if (!free(param))
                hnf(body, bodCtx.updated(param, arg, ctx))
              else {
                val newName = s"%$i"
                val newBody = body.unpack(renamer(param, newName))
                val newCtx  = bodCtx.rename(param, newName).updated(newName, arg, ctx)
                hnf(newBody, newCtx)
              }
            }
          }
        )
      )

    def lam(param: String, body: NP): Eval[HNF] =
      Eval.now(Rep[LamNorm, (NP, Context)](_.lam(param, (body, ctx - param))))
  }

  private def hnf(np: NP, ctx: Context): Eval[HNF] =
    np.unpack(Lam.proTraverse.rmap(runHNF(ctx))(x => (_, _) => x))

  private val nfBuild = Builder[LamNorm, (NP, Context)] { case ((np, ctx), k) => hnf(np, ctx).value(k) }

  def apply(p: lambda.untyped.Lam.T): lambda.untyped.LamNorm.T = nfBuild.unfold((prepare(p), Context()))

  def renamer(oldName: String, newName: String): NormPrep[NP, NP] =
    Lam.proTraverse.rmap(new NormPrep[NP, NP] {
      def vari(name: String) = NP.vari(if (name == oldName) newName else name)

      def app(f: NP, arg: NP) =
        NP.app(f.unpack(renamer(oldName, newName)), arg.unpack(renamer(oldName, newName)))

      def lam(param: String, body: NP) =
        NP.lam(param, if (oldName == param) body else body.unpack(renamer(oldName, newName)))
    })(fnp => (i, s) => fnp(i, if (s(oldName)) s - oldName + newName else s))

  private val increaseFree: Option[Int] => Option[Int] = {
    case None    => Some(1)
    case Some(i) => Some(i + 1)
  }
  private val decreaseFree: Option[Int] => Option[Int] = {
    case Some(i) if i > 0 => Some(i - 1)
    case _                => None
  }

  final case class Context(bindings: Map[String, (NP, Context)] = Map.empty, free: Map[String, Int] = Map.empty) {
    private def updatedFree(exp: NP, ctx: Context)(f: Option[Int] => Option[Int]): Map[String, Int] =
      (prepared(exp)._2 union ctx.free.keySet).foldLeft(free)(_.updatedWith(_)(f))

    def updated(key: String, exp: NP, ctx: Context): Context = (this - key).added(key, exp, ctx)

    def added(key: String, exp: NP, ctx: Context): Context =
      Context(bindings.updated(key, (exp, ctx)), updatedFree(exp, ctx)(increaseFree))

    def rename(key: String, newKey: String): Context = if (!free.contains(key)) this
    else {
      val newBindings = bindings.view.mapValues { case (np, ctx) =>
        (np.unpack(renamer(key, newKey)), ctx.rename(key, newKey))
      }.toMap
      val newFree     = (free - key).updated(newKey, free(key))
      Context(newBindings, newFree)
    }

    def -(key: String): Context = {
      val newFree = bindings.get(key).fold(free) { case (exp, ctx) => updatedFree(exp, ctx)(decreaseFree) }
      Context(bindings - key, newFree)
    }
  }

}

object SimpleNorm {
  def apply(lam: Lam.T): LamNorm.T = nf(lam, Map.empty, 1).value.norm

  case class Res(norm: LamNorm.T, idx: Int, free: Set[String])

  lazy val norm = LamNorm.mk

  private def nf(lam: Lam.T, ctx: Ctx, start: Int): Eval[Res] =
    lam
      .unpack(runNF(ctx, start))
      .flatTap(res => Eval.always(println(s"[${lam.show}]${showCtx(ctx)} = ${res.norm.show}")))

  def showCtx(ctx: Ctx) = ctx.view.map { case (i, v) => s"$i:${v._1.show}" }.mkString("{", ", ", "}")
  type Ctx = Map[String, (LamNorm.T, Set[String])]

  private def runNF(ctx: Ctx, start: Int) =
    new Lam.Match[Eval[Res]] {
      def vari(name: String): Eval[Res] =
        Eval.now(ctx.get(name) match {
          case Some((term, free)) => Res(term, start, free)
          case None               => Res(norm.vapp(name), start, Set(name))
        })

      def app(f: Lam.T, arg: Lam.T): Eval[Res] =
        for {
          Res(fn, idx1, ffree)  <- nf(f, ctx, start)
          Res(argn, idx, afree) <- nf(arg, ctx, idx1)
          matcher                =
            new LamNorm.Match[Eval[Res]] {
              def varApp(name: String, args: Vector[LamNorm.T]) =
                Eval.now(Res(norm.varApp(name, args :+ argn), idx, ffree union afree))

              def lam(param: String, body: LamNorm.T): Eval[Res] = {
                if (afree(param) && ffree(param)) {
                  val newName = s"%$idx"
                  val newIdx  = idx + 1
                  nf(body.rename(param, newName).lam, Map(newName -> (argn -> afree)), newIdx)
                } else {
                  println(s"${body.show} --> ${body.lam.show} // $param: {${argn.show} :: ${arg.show}}")
                  nf(body.lam, Map(param -> (argn, afree)), idx)
                }
              }
            }
          result                <- fn.unpack(matcher)
        } yield result

      def lam(param: String, body: Lam.T): Eval[Res] =
        nf(body, ctx - param, start).map { case Res(bn, idx, free) =>
          Res(norm.lam(param, bn), idx, free - param)
        }
    }
}
