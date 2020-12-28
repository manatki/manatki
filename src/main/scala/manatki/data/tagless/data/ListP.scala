package manatki.data.tagless.data
import cats.{Applicative, Functor, Show}
import manatki.data.tagless.{Builder, FunK2, Layer, ProCorep, ProTraverse, Rep}
import cats.syntax.show._
import cats.instances.string._
import manatki.data.tagless.FunK2.{Arb1, Arb2}
import manatki.data.tagless.ProTraverse.Tab

trait ListP[-A, -I, +O] extends Cons[A, I, O] with Nil[O]

object ListP {
  def apply[A](as: A*): XList[A] = fromSeq(as)

  def fromSeq[A](as: Seq[A]): XList[A] =
    Builder[ListP[A, -*, +*], Seq[A]] {
      case (a +: as, p) => p.cons(a, as)
      case (Seq(), p)   => p.nil
    }.unfold(as)

  implicit def corepresentable[I]: ProTraverse[ListP[I, -*, +*]] = new ProTraverse[ListP[I, -*, +*]] {
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

  implicit val functor: Functor[XList] = new Functor[XList] {
    def map[A, B](fa: XList[A])(f: A => B): XList[B] =
      fa.contramapK2[ListP[B, -*, +*]](FunK2(contramap(f)(_)))
  }
}
