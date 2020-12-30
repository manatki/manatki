package manatki.data.lambda.untyped

import cats.syntax.traverse._
import cats.{Applicative, Eval}
import io.circe.Json
import manatki.data
import manatki.data.lambda
import manatki.data.lambda.untyped
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

object LamNorm extends ProfData[LamNorm] with ProfFoldOps {

  object nat {
    import mk._
    def zero        = lam("s", lam("z", vapp("z")))
    def one         = lam("s", lam("z", vapp("s", vapp("z"))))
    def int(x: Int) = lam("s", lam("z", (1 to x).foldLeft(vapp("z"))((t, _) => vapp("s", t))))
  }

  override val foldOps = new FoldOps[Fold] {
    val lam  = new Fold[Lam.T] {
      override def varApp(name: String, args: Vector[Lam.T]): Lam.T = args.foldLeft(Lam.mk.vari(name))(Lam.mk.app)
      override def lam(param: String, body: Lam.T): Lam.T           = Lam.mk.lam(param, body)
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

  implicit val foldFunctor = representableK.instance

  trait ProTab[F[_], A, B, C, P[-x, +y] <: LamNorm[x, y]] extends Tab[F, A, B, C, P] with LamNorm[A, C] {
    def varApp(name: String, args: Vector[A]): C =
      right(args.traverse(left).map(bs => rep(_.varApp(name, bs))))

    def lam(param: String, body: A): C = right(left(body).map(b => rep(_.lam(param, b))))
  }

  implicit val showFold: Fold[String] = new Fold[String] {
    def varApp(name: String, args: Vector[String]): String =
      if (args.isEmpty) name else args.mkString(name + " (", ") (", ")")

    def lam(param: String, body: String): String = s"λ $param.$body"
  }
}

trait Lam[-I, +O] {
  def vari(name: String): O
  def app(f: I, arg: I): O
  def lam(param: String, body: I): O
}

object Lam extends ProfData[Lam] {
  implicit class LamOps(private val x: T) extends AnyVal {
    def apply(ys: T*): T = ys.foldLeft(x)(mk.app)
//    def normal: LamNorm.T = normalize(x)(Map()).value
    def show             = x.foldS(showFold)._1

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

  type HNF = Rep[LamNorm, (NP, Ctx)]

  type Ctx = Context[String, NP]
  val Ctx: Ctx = Context[String, NP]()

  private def runHNF(ctx: Ctx): Lam[NP, Eval[HNF]] = new Lam[NP, Eval[HNF]] {
    final def vari(name: String): Eval[HNF] = ctx.bindings.get(name) match {
      case None            => Eval.now(Rep[LamNorm, (NP, Ctx)](_.vapp(name)))
      case Some((np, ctx)) => hnf(np, ctx)
    }

    def app(f: NP, arg: NP): Eval[HNF] =
      hnf(f, ctx).flatMap(
        _(
          new LamNorm[(NP, Ctx), Eval[HNF]] {
            def varApp(name: String, args: Vector[(NP, Ctx)]): Eval[HNF] =
              Eval.now(Rep[LamNorm, (NP, Ctx)](_.varApp(name, args :+ (arg, ctx))))

            def lam(param: String, body: (NP, Ctx)): Eval[HNF] = {
              val (i, free)          = prepared(arg)
              val (newBody, newName) =
                if (!free(param)) (body._1, param)
                else {
                  val newName = s"%$i"
                  (body._1.unpack(rename(param, newName)), newName)
                }

              hnf(newBody, body._2.updated(newName, arg, ctx))
            }
          }
        )
      )

    def lam(param: String, body: NP): Eval[HNF] = Eval.now(Rep[LamNorm, (NP, Ctx)](_.lam(param, (body, ctx))))
  }

  private def hnf(np: NP, ctx: Ctx): Eval[HNF] =
    np.unpack(Lam.proTraverse.rmap(runHNF(ctx))(x => (_, _) => x))

  private val nfBuild = Builder[LamNorm, (NP, Ctx)] { case ((np, ctx), k) => hnf(np, ctx).value(k) }

  def apply(p: lambda.untyped.Lam.T): lambda.untyped.LamNorm.T = nfBuild.unfold((prepare(p), Ctx))

  def rename(oldName: String, newName: String): NormPrep[NP, NP] =
    Lam.proTraverse.rmap(new NormPrep[NP, NP] {
      def vari(name: String) = NP.vari(if (name == oldName) newName else name)

      def app(f: NP, arg: NP) =
        NP.app(f.unpack(rename(oldName, newName)), arg.unpack(rename(oldName, newName)))

      def lam(param: String, body: NP) =
        NP.lam(param, if (oldName == param) body else body.unpack(rename(oldName, newName)))
    })(fnp => (i, s) => fnp(i, if (s(oldName)) s - oldName + newName else s))
}

final case class Context[Key, +Exp](bindings: Map[Key, (Exp, Context[Key, Exp])] = Map.empty[Key, Nothing]) {
  def updated[E1 >: Exp](key: Key, exp: E1, ctx: Context[Key, E1]): Context[Key, E1] =
    copy(bindings = bindings.updated(key, (exp, ctx)))
}
