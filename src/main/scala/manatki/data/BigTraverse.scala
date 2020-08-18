package manatki.data
package scalagroup

import cats.data.{Nested, ZipList}
import cats.{Applicative, Apply, Functor, Id, Monad, Parallel, ~>}
import shapeless._
import tofu.syntax.monadic._
import cats.instances.list._

object BigTraverse extends App {

  case class Big[F[_]](
      field1: F[String],
      field2: F[Int],
      field3: F[Boolean],
      field4: F[String],
      field5: F[Int],
      field6: F[Boolean],
      field7: F[String],
      field8: F[Int],
      field9: F[Boolean],
      field10: F[String],
      field11: F[Int],
      field12: F[Boolean],
      field13: F[String],
      field14: F[Int],
      field15: F[Boolean],
      field16: F[String],
      field17: F[Int],
      field18: F[Boolean],
      field19: F[String],
      field20: F[Int],
      field21: F[Boolean],
      field22: F[String],
      field23: F[Int],
      field24: F[Boolean],
      field25: F[String],
      field26: F[Int],
      field27: F[Boolean],
      field28: F[String],
      field29: F[Int],
  )

  //  println(
  //    (1 to 29)
  //      .zip(LazyList.continually(LazyList("String", "Int", "Boolean")).flatten)
  //      .map { case (i, typ) => s"field$i: F[$typ]," }
  //      .mkString("case class Big[F[_]](\n", "\n", "\n)")
  //  )

//  println(
//    (1 to 29)
//      .zip(
//        LazyList.continually(LazyList("""LazyList("lol", "kek")""", "LazyList(1, 2)", "LazyList(true, false)")).flatten
//      )
//      .map { case (i, v) => s"field$i = $v," }
//      .mkString("val test = Big[LazyList](\n", "\n", "\n)")
//  )

  type Cmp[F[_], G[_], A] = F[G[A]]

  trait TraverseMonoHK[D[f[_]], F[_], G[_], H[_]] {
    def traverseMonoHK(d: D[F], f: F ~> Cmp[G, H, *]): G[D[H]]
    final def traverseHKD(d: D[F])(f: F ~> Cmp[G, H, *]): G[D[H]] = traverseMonoHK(d, f)
  }

  object TraverseMonoHK {
    implicit def genericTraverseMonoHK[C[_[_]], F[_], G[_]: Functor, H[_], L1 <: HList, L2 <: HList](implicit
        gen1: Generic.Aux[C[F], L1],
        gen2: Generic.Aux[C[H], L2],
        ta: TraverseAs[L1, L2, F, G, H]
    ): TraverseMonoHK[C, F, G, H] = (cf, f) => ta.traverseAs(gen1.to(cf), f).map(gen2.from)
  }

  trait TraverseAs[L1 <: HList, L2 <: HList, F[_], G[_], H[_]] {
    def traverseAs(l1: L1, f: F ~> Cmp[G, H, *]): G[L2]
  }

  object TraverseAs {
    implicit def traverseNil[F[_], G[_]: Applicative, H[_]]: TraverseAs[HNil, HNil, F, G, H] =
      (_, _) => (HNil: HNil).pure[G]

    implicit def traverseCons[F[_], G[_]: Apply, H[_], T1 <: HList, T2 <: HList, A](implicit
        ta: TraverseAs[T1, T2, F, G, H]
    ): TraverseAs[F[A] :: T1, H[A] :: T2, F, G, H] = {
      case (fa :: t1, f) => f(fa).map2(ta.traverseAs(t1, f))(_ :: _)
    }
  }

  def parSequenceHKD[C[f[_]], F[_], P[_]](
      cf: C[F]
  )(implicit FP: Parallel.Aux[F, P], tm: TraverseMonoHK[C, F, P, Id]): F[C[Id]] =
    FP.sequential(
      tm.traverseMonoHK(
        cf,
        new (F ~> Cmp[P, Id, *]) {
          def apply[A](fa: F[A]): P[A] = FP.parallel(fa)
        }
      )
    )

  val test = Big[LazyList](
    field1 = LazyList("lol", "kek"),
    field2 = LazyList(1, 2),
    field3 = LazyList(true, false),
    field4 = LazyList("lol", "kek"),
    field5 = LazyList(1, 2),
    field6 = LazyList(true, false),
    field7 = LazyList("lol", "kek"),
    field8 = LazyList(1, 2),
    field9 = LazyList(true, false),
    field10 = LazyList("lol", "kek"),
    field11 = LazyList(1, 2),
    field12 = LazyList(true, false),
    field13 = LazyList("lol", "kek"),
    field14 = LazyList(1, 2),
    field15 = LazyList(true, false),
    field16 = LazyList("lol", "kek"),
    field17 = LazyList(1, 2),
    field18 = LazyList(true, false),
    field19 = LazyList("lol", "kek"),
    field20 = LazyList(1, 2),
    field21 = LazyList(true, false),
    field22 = LazyList("lol", "kek"),
    field23 = LazyList(1, 2),
    field24 = LazyList(true, false),
    field25 = LazyList("lol", "kek"),
    field26 = LazyList(1, 2),
    field27 = LazyList(true, false),
    field28 = LazyList("lol", "kek"),
    field29 = LazyList(1, 2),
  )

  import ZipSeq.parallel

  parSequenceHKD(test).foreach(println)

}
