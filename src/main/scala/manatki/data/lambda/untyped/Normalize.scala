package manatki.data.lambda.untyped
import cats.Eval
import io.circe.Json
import manatki.data.lambda
import manatki.data.lambda.untyped.Lam.NormPrep
import manatki.data.tagless._
import tofu.data.ICalcM
import tofu.data.calc.CalcM

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
