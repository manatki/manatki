package manatki.data.tagless
import cats.arrow.Profunctor
import cats.syntax.functor._

trait Layered[P[_, _]] extends Profunctor[P] {
  type L[A] = Layer[P, A]
  def tagless[A, B](f: L[A] => B): P[A, B]
  private implicit val functor = Layer.functor[P](this)
  def asLayer[A, B](pab: P[A, B]): L[A] => B = _.cont(pab)

  def zip[A, B, C, D](pab: P[A, B], pcd: P[C, D]): P[(A, C), (B, D)] =
    tagless(lac => (lac.map(_._1).cont(pab), lac.map(_._2).cont(pcd)))

}
