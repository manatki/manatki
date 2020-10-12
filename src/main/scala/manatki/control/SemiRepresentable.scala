package manatki.control

import tofu.higherKind.RepK
import cats._
import tofu.higherKind.UnitK

case class Chpok[F[_]](
    zoo: String,
    boo: Long,
    lol: F[String],
    kek: F[Int],
    cheburek: F[Long]
)

object Chpok {
  implicit val semirep: SemiRepresentableK[Chpok] = new SemiRepresentableK[Chpok] {
    def semiTabulate[F[_]](uni: Chpok[UnitK], src: RepK[Chpok, *] ~> F): Chpok[F] =
      Chpok(
        uni.zoo,
        uni.boo,
        src(RepK.mk(_.lol)),
        src(RepK.mk(_.kek)),
        src(RepK.mk(_.cheburek))
      )

    def untr[F[_]](uf: Chpok[F]) =
      Chpok[UnitK](
        uf.zoo,
        uf.boo,
        (),
        (),
        ()
      )
  }
}

trait SemiRepresentableK[U[_[_]]] {
  def semiTabulate[F[_]](uni: U[UnitK], src: RepK[U, *] ~> F): U[F]
  def untr[F[_]](uf: U[F]): U[UnitK]
  final def index[F[_], A](uf: U[F], rep: RepK[U, A]): F[A] = rep(uf)
}
