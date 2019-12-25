package manatki.data.layer.skin

import cats.arrow.Profunctor
import cats.syntax.functor._

trait Skinny[P[_, _]] extends Profunctor[P] {
  def tagless[A, B](f: Skin[P, A] => B): P[A, B]

  type S[A] = Skin[P, A]

  val construct: P[Skin.Fix[P], Skin.Fix[P]] = tagless(lf => Skin.fix(lf))

  private implicit val functor               = Skin.functor[P](this)
  def asSkin[A, B](pab: P[A, B]): S[A] => B = _.cont(pab)

  def zip[A, B, C, D](pab: P[A, B], pcd: P[C, D]): P[(A, C), (B, D)] =
    tagless(lac => (lac.map(_._1).cont(pab), lac.map(_._2).cont(pcd)))
}


