package manatki.data.tagless.data
import cats.{Applicative, Show}
import manatki.data.tagless.ProCorepresentable.{LMap, Tab}
import manatki.data.tagless.{Builder, Layer, ProCorepresentable, ProTraverse, Rep}
import cats.syntax.show._
import cats.instances.string._
import manatki.data.tagless.ProTraverse.ProTrav

trait ListP[-A, -I, +O] extends Cons[A, I, O] with Nil[O]

object ListP {
  def apply[A](as: A*): XList[A] = fromSeq(as)

  def fromSeq[A](as: Seq[A]): XList[A] =
    Builder[ListP[A, -*, +*], Seq[A]] {
      case (a +: as, p) => p.cons(a, as)
      case (Seq(), p)   => p.nil
    }.unfold(as)

  implicit def corepresentable[I]: ProTraverse[ListP[I, *, *]] = new ProTraverse[ListP[I, *, *]] {
    def tabulate[A, B](k: Rep[ListP[I, A, *]] => B): ListP[I, A, B] =
      new Tab[A, B, ListP[I, *, *]](k) with Tabulate[I, A, B, ListP[I, *, *]]

    def leftMap[A, B, C](fab: ListP[I, A, B])(f: C => A): ListP[I, C, B] =
      new LMap[A, B, C, ListP[I, *, *]](fab, f) with LeftMap[I, A, B, C, ListP[I, *, *]]

    def protraverse[F[_]: Applicative, A, B](p: ListP[I, A, B]): ListP[I, F[A], F[B]] =
      new ProTrav[F, A, B, ListP[I, *, *]](p) with Trav[F, I, A, B, ListP[I, *, *]]
  }

  trait Tabulate[I, A, B, P[x, y] <: ListP[I, x, y]]
      extends Tab[A, B, P] with ListP[I, A, B] with Nil.Tabulate[A, B, P] with Cons.Tabulate[I, A, B, P]

  trait LeftMap[I, A, B, C, P[x, y] <: ListP[I, x, y]]
      extends LMap[A, B, C, P] with ListP[I, C, B] with Nil.LeftMap[A, B, C, P] with Cons.LeftMap[I, A, B, C, P]

  trait Trav[F[_], I, A, B, P[x, y] <: ListP[I, x, y]]
      extends ProTrav[F, A, B, P] with ListP[I, F[A], F[B]] with Nil.Trav[F, A, B, P] with Cons.Trav[F, I, A, B, P]

  implicit def showInstance[A: Show]: Show[XList[A]] =
    "List(" + _.fold(new ListP[A, String, String] {
      def nil: String = ")"
      def cons(a: A, y: String): String = y match {
        case ")" => show"$a)"
        case _   => show"$a, $y"
      }
    })
}
