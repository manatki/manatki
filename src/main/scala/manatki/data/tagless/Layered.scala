package manatki.data.tagless
import cats.arrow.Profunctor
import cats.syntax.functor._

trait Layered[P[_, _]] extends Profunctor[P] {
  def tagless[A, B](f: Layer[P, A] => B): P[A, B]

  type L[A] = Layer[P, A]

  val construct: P[Layer.Fix[P], Layer.Fix[P]] = tagless(lf => Layer.fix(lf))

  private implicit val functor               = Layer.functor[P](this)
  def asLayer[A, B](pab: P[A, B]): L[A] => B = _.cont(pab)

  def zip[A, B, C, D](pab: P[A, B], pcd: P[C, D]): P[(A, C), (B, D)] =
    tagless(lac => (lac.map(_._1).cont(pab), lac.map(_._2).cont(pcd)))
}

object Layered {


}
