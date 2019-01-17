package manatki.data
import cats.syntax.apply._
import cats.{Applicative, Eval}

final case class NaturalTree[A](x: Eval[A], left: Eval[NaturalTree[A]], right: Eval[NaturalTree[A]]) {
  def apply(ix: BigInt): Eval[A] =
    if (ix == BigInt(0)) x else if (ix.testBit(0)) left.flatMap(_(ix / 2)) else right.flatMap(_(ix / 2 - 1))
}

object NaturalTree {
  //naturals(x) = x
  val naturals: NaturalTree[BigInt] = {
    def go(n: BigInt, r: BigInt): NaturalTree[BigInt] = {
      val r2 = r * 2
      NaturalTree(Eval.now(n), Eval.later(go(n + r, r2)), Eval.later(go(n + r2, r2)))
    }
    go(0, 1)
  }

  implicit val naturalTreeInstance: Applicative[NaturalTree] = new Applicative[NaturalTree] {
    def pure[A](x: A): NaturalTree[A] = {
      lazy val tree: NaturalTree[A] = NaturalTree(Eval.now(x), sub, sub)
      lazy val sub                  = Eval.later(tree)
      tree
    }
    def ap[A, B](ff: NaturalTree[A => B])(fa: NaturalTree[A]): NaturalTree[B] = map2(ff, fa)(_(_))

    override def map2[A, B, Z](fa: NaturalTree[A], fb: NaturalTree[B])(f: (A, B) => Z): NaturalTree[Z] =
      NaturalTree(fa.x.map2(fb.x)(f), fa.left.map2(fb.left)(map2(_, _)(f)), fa.right.map2(fb.right)(map2(_, _)(f)))

    override def productL[A, B](fa: NaturalTree[A])(fb: NaturalTree[B]): NaturalTree[A] = fa
    override def productR[A, B](fa: NaturalTree[A])(fb: NaturalTree[B]): NaturalTree[B] = fb

    override val unit: NaturalTree[Unit] = {
      val sub = Eval.later(unit)
      NaturalTree(Eval.now(()), sub, sub)
    }

    override def map[A, B](fa: NaturalTree[A])(f: A => B): NaturalTree[B] =
      NaturalTree(fa.x.map(f), fa.left.map(map(_)(f)), fa.right.map(map(_)(f)))

    override def replicateA[A](n: Int, fa: NaturalTree[A]): NaturalTree[List[A]] =
      NaturalTree(fa.x.map(List.fill(n)), fa.left.map(replicateA(n, _)), fa.right.map(replicateA(n, _)))
  }
}
