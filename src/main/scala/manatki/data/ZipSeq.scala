package manatki.data

import cats.{Applicative, Monad, Parallel, ~>}
import tofu.syntax.funk
import tofu.syntax.funk.funK

sealed trait ZipSeq[+A] {
  def toLazyList: LazyList[A] =
    this match {
      case ZipSeq.Continually(a) => LazyList.continually(a)
      case ZipSeq.Finite(vals)   => vals
    }
}

object ZipSeq {
  case class Continually[+A](a: A)         extends ZipSeq[A]
  case class Finite[+A](vals: LazyList[A]) extends ZipSeq[A]

  implicit val applicative: Applicative[ZipSeq] = new Applicative[ZipSeq] {
    def pure[A](x: A): ZipSeq[A]                               = Continually(x)
    def ap[A, B](ff: ZipSeq[A => B])(fa: ZipSeq[A]): ZipSeq[B] = map2(ff, fa)(_(_))

    override def map2[A, B, Z](fa: ZipSeq[A], fb: ZipSeq[B])(f: (A, B) => Z): ZipSeq[Z] =
      fa match {
        case Continually(a) =>
          fb match {
            case Continually(b) => Continually(f(a, b))
            case Finite(bs)     => Finite(bs.map(f(a, _)))
          }
        case Finite(as)     =>
          fb match {
            case Continually(b) => Finite(as.map(f(_, b)))
            case Finite(bs)     => Finite(as.lazyZip(bs).map(f))
          }
      }
  }

  implicit val parallel: Parallel.Aux[LazyList, ZipSeq] = new Parallel[LazyList] {
    import cats.instances.lazyList._
    type F[A] = ZipSeq[A]

    def applicative: Applicative[ZipSeq] = implicitly

    def monad: Monad[LazyList] = implicitly

    def sequential: ZipSeq ~> LazyList = funK(_.toLazyList)

    def parallel: LazyList ~> ZipSeq = funK(Finite(_))
  }
}
