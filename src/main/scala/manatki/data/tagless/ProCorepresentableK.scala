package manatki.data.tagless
import cats.arrow.FunctionK
import cats.tagless.FunctorK
import cats.~>
import manatki.data.tagless.ProCorepresentable.tabulate
import manatki.syntax.funK
import tofu.higherKind.RepK

trait ProfunctorK[P[_[_], _[_]]] {
  def leftMapK[I[_], J[_], O[_]](pio: P[I, O])(fk: J ~> I): P[J, O]
  def rightMapK[I[_], O[_], Q[_]](pio: P[I, O])(fk: O ~> Q): P[I, Q]

  def dimapK[I[_], O[_], J[_], Q[_]](pio: P[I, O])(fji: J ~> I)(foq: O ~> Q): P[J, Q] =
    leftMapK(rightMapK(pio)(foq))(fji)
}

trait ProCorepresentableK[P[_[_], _[_]]] extends ProfunctorK[P] {
  def tabulate[I[_], O[_]](hom: RepK[P[I, *[_]], *] ~> O): P[I, O]

  override def rightMapK[I[_], O[_], Q[_]](fab: P[I, O])(f: O ~> Q): P[I, Q] =
    tabulate(funK(rep => f(rep(fab))))

  def repFunctorK[A]: FunctorK[λ[i[_] => RepK[P[i, *[_]], A]]] =
    new FunctorK[λ[i[_] => RepK[P[i, *[_]], A]]] {
      def mapK[F[_], G[_]](af: RepK[P[F, *[_]], A])(fk : F ~> G): RepK[P[G, *[_]], A] =
        af(leftMapK(tabulate(FunctionK.id[RepK[P[G, *[_]], *]]))(fk))
    }
}

object ProCorepresentableK{
  def tabulateK[P[_[_], _[_]], F[_], G[_]](k: RepK[P[F, *[_]], *] ~> G)(implicit P: ProCorepresentableK[P]): P[F, G] =
    P.tabulate(k)

  def constructK[P[-_[_], +_[_]]: ProCorepresentableK]: P[Layer1[P, *], Layer1[P, *]] =
    tabulateK[P, Layer1[P, *], Layer1[P, *]](funK(_(constructK)))
}
