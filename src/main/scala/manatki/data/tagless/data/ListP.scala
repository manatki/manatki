package manatki.data.tagless.data
import cats.syntax.show._
import cats.{Applicative, Functor, Show}
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless.{Builder, FunK2, ProTraverse, ProfData1}
import tofu.higherKind.derived.representableK

trait ListP[-A, -I, +O] extends Cons[A, I, O] with Nil[O]

object ListP extends ProfData1[ListP] {
  def apply[A](as: A*): XList[A] = fromSeq(as)

  def fromSeq[A](as: Seq[A]): XList[A] =
    Builder[ListP[A, -*, +*], Seq[A]] {
      case (a +: as, p) => p.cons(a, as)
      case (Seq(), p)   => p.nil
    }.unfold(as)

  trait FoldOps[A, F[_]] {
    def sum(implicit A: Numeric[A]): F[A]
    def length: F[Long]
    def toList: F[List[A]]
  }

  implicit def opsFunctor[A] = representableK.instance

  def foldOps[A] = new FoldOps[A, Fold[A, *]] {
    override def sum(implicit A: Numeric[A]) = new Fold[A, A] {
      def cons(x: A, y: A): A = A.plus(x, y)
      def nil: A              = A.zero
    }

    override def length = new Fold[Any, Long] {
      def cons(a: Any, y: Long): Long = y + 1
      def nil: Long                   = 0
    }

    def toList = new Fold[A, List[A]] {
      def cons(a: A, y: List[A]): List[A] = a :: y
      def nil: List[A]                    = List.empty
    }
  }

  implicit def proTraverse[I]: ProTraverse[ListP[I, -*, +*]] = new ProTraverse[ListP[I, -*, +*]] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): ListP[I, A, C] =
      new Tab[F, A, B, C, ListP[I, -*, +*]](left, right) with TabTraverse[I, F, A, B, C, ListP[I, -*, +*]]
  }

  trait TabTraverse[I, F[_], A, B, C, P[-x, +y] <: ListP[I, x, y]]
      extends Tab[F, A, B, C, P] with ListP[I, A, C] with Nil.TabTraverse[F, A, B, C, P]
      with Cons.TabTraverse[I, F, A, B, C, P]

  implicit def showInstance[A: Show]: Show[XList[A]] =
    "List(" + _.fold(new ListP[A, String, String] {
      def nil: String                   = ")"
      def cons(a: A, y: String): String = y match {
        case ")" => show"$a)"
        case _   => show"$a, $y"
      }
    })

  private def contramap[A, B, I, O](f: A => B)(l: ListP[B, I, O]) = new ListP[A, I, O] {
    def nil: O              = l.nil
    def cons(a: A, y: I): O = l.cons(f(a), y)
  }

  implicit val functor: Functor[T] = new Functor[T] {
    def map[A, B](fa: T[A])(f: A => B): T[B] =
      fa.contramapK2[ListP[B, -*, +*]](FunK2(contramap(f)(_)))
  }
}
