package manatki.data.lambda.untyped

import cats.syntax.traverse._
import cats.{Applicative, Eval}
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless.{Layer, ProTraverse, ProfData}
import tofu.higherKind.derived.representableK
import tofu.syntax.monadic._

trait LamNorm[-I, +O] {
  def varApp(name: String, args: Vector[I]): O
  def lam(param: String, body: I): O

  final def vapp(name: String, args: I*) = varApp(name, args.toVector)
}

object LamNorm extends ProfData[LamNorm] {

  object nat {
    import mk._
    def zero        = lam("s", lam("z", vapp("z")))
    def one         = lam("s", lam("z", vapp("s", vapp("z"))))
    def int(x: Int) = lam("s", lam("z", (1 to x).foldLeft(vapp("z"))((t, _) => vapp("s", t))))
  }

  override lazy val foldOps = new FoldOps[Fold] {
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
    def apply(ys: T*): T  = ys.foldLeft(x)(mk.app)
//    def normal: LamNorm.T = normalize(x)(Map()).value
    def show              = x.foldS(showFold)._1

  }

  implicit class stringOps(private val name: String) extends AnyVal {
    def lam(body: T): T                 = mk.lam(name, body)
    def lam(names: String*)(body: T): T = (name +: names).foldRight(body)(mk.lam)
    def vari: T                         = mk.vari(name)
  }

  object nat {
    def int(x: Int) = "s".lam("z")((1 to x).foldLeft("z".vari)((t, _) => "s".vari(t)))
    val zero        = int(0)
    def plus        = "a".lam("b", "s", "z")("a".vari("s".vari, "b".vari("s".vari, "z".vari)))
    def mul         = "x".lam("y")("x".vari(plus("y".vari), zero))
  }

  type NormPrep[-I, +O] = Lam[I, (Long, Set[String]) => O]

  type NP = Layer[NormPrep]

//  private def prepare(t: T, i: Long): Eval[(NP, Long)]

//  def normalize(t: T)(ctx: Map[String, Eval[LamNorm.T]]): Eval[LamNorm.T] =
//    t.unpack(new Match[Eval[LamNorm.T]] {
//      def vari(name: String): Eval[LamNorm.T] =
//        ctx.getOrElse(name, Eval.now(LamNorm.mk.vapp(name)))
//
//      def app(f: T, arg: T): Eval[LamNorm.T] = normalize(f)(ctx).flatMap(_.unpack {
//        new LamNorm.Match[Eval[LamNorm.T]] {
//          def varApp(name: String, args: Vector[LamNorm.T]): Eval[LamNorm.T] =
//            normalize(arg)(ctx).map(x => LamNorm.mk.varApp(name, args :+ x))
//
//          def lam(param: String, body: LamNorm.T): Eval[LamNorm.T] =
//            normalize(body.lam)(ctx.updated(param, normalize(arg)(ctx).memoize))
//        }
//      })
//
//      def lam(param: String, body: T): Eval[LamNorm.T] = normalize(body)(ctx - param).map(LamNorm.mk.lam(param, _))
//    })

  val showFold = new Fold[(String, Int)] {
    def vari(name: String) = (name, 1)

    private def wrap(x: (String, Int), pos: Int)  = if (x._2 > pos) s"(${x._1})" else x._1
    def app(f: (String, Int), arg: (String, Int)) = (s"${wrap(f, 3)} ${wrap(arg, 2)}", 3)

    def lam(param: String, body: (String, Int)) = (s"λ $param.${body._1}", 4)
  }

  val foldOps = new FoldOps[Fold] {}

  val proTraverse = new ProTraverse[Lam] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): Lam[A, C] =
      new Tab(left, right) with ProTab[F, A, B, C, Lam]
  }

  class FoldOps[F[_]]

  val foldFunctor = representableK.instance

  trait ProTab[F[_], A, B, C, P[-x, +y] <: Lam[x, y]] extends Tab[F, A, B, C, P] with Lam[A, C] {
    def vari(name: String): C          = right(rep(_.vari(name)).pure)
    def app(f: A, arg: A): C           = right(left(f).map2(left(arg))((fb, argb) => rep(_.app(fb, argb))))
    def lam(param: String, body: A): C = right(left(body).map(b => rep(_.lam(param, b))))
  }
}
