package manatki.data.tagless
import cats.arrow.FunctionK
import cats.tagless.FunctorK
import cats.{Applicative, ~>}
import manatki.syntax.funK
import simulacrum.typeclass
import tofu.higherKind.RepK

@typeclass
trait ProK[P[-_[_], +_[_]]] {
  def leftMapK[I[_], J[_], O[_]](pio: P[I, O])(fk: J ~> I): P[J, O]
  def rightMapK[I[_], O[_], Q[_]](pio: P[I, O])(fk: O ~> Q): P[I, Q]

  def dimapK[I[_], O[_], J[_], Q[_]](pio: P[I, O])(fji: J ~> I)(foq: O ~> Q): P[J, Q] =
    leftMapK(rightMapK(pio)(foq))(fji)
}

@typeclass
trait ProCorepK[P[_[_], _[_]]] extends ProK[P] {
  def tabulateK[I[_], O[_]](hom: RepK[P[I, *[_]], *] ~> O): P[I, O]

  override def rightMapK[I[_], O[_], Q[_]](fab: P[I, O])(f: O ~> Q): P[I, Q] =
    tabulateK(funK(rep => f(rep(fab))))

  def repFunctorK[A]: FunctorK[λ[i[_] => RepK[P[i, *[_]], A]]] =
    new FunctorK[λ[i[_] => RepK[P[i, *[_]], A]]] {
      def mapK[F[_], G[_]](af: RepK[P[F, *[_]], A])(fk: F ~> G): RepK[P[G, *[_]], A] =
        af(leftMapK(tabulateK(FunctionK.id[RepK[P[G, *[_]], *]]))(fk))
    }
}

object ProCorepK {
  def tabulateK[P[_[_], _[_]], F[_], G[_]](k: RepK[P[F, *[_]], *] ~> G)(implicit P: ProCorepK[P]): P[F, G] =
    P.tabulateK(k)

  def constructK[P[-_[_], +_[_]]: ProCorepK]: P[Platform[P, *], Platform[P, *]] =
    tabulateK[P, Platform[P, *], Platform[P, *]](funK(_(constructK)))
}

@typeclass
trait ProTraverseK[P[_[_], _[_]]] extends ProCorepK[P] {
  def proTraverseK[F[_]: Applicative, I[_], O[_]](pio: P[I, O]): P[λ[a => F[I[a]]], λ[a => F[O[a]]]]
}


