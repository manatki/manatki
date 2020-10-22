import io.circe.ACursor
import izumi.reflect.macrortti.LightTypeTagRef.Lambda
import io.circe.{Decoder, HCursor}
import cats.data.Const
import shapeless.ops.fin
import tofu.higherKind.Function2K
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import algebra.ring.Field
import tofu.syntax.funk
import cats.Applicative
import tofu.higherKind.RepK
import cats.arrow.FunctionK
import shapeless.ops.nat.Prod
import tofu.higherKind.RepresentableK
import cats._
import tofu.syntax.monadic._
import tofu.syntax.funk._
import cats.tagless.syntax.functorK._
import cats.instances.all._
import tofu.syntax.monoidalK._
type Category = Long

case class Product[F[_]](
    name: F[String],
    category: F[Option[Category]],
    price: F[BigDecimal]
)

implicit val productRepresentable: RepresentableK[Product] =
  new RepresentableK[Product] {
    def tabulate[F[_]](fr: FunctionK[RepK[Product, *], F]): Product[F] =
      Product(
        name = fr(RepK[Product](_.name)),
        category = fr(RepK[Product](_.category)),
        price = fr(RepK[Product](_.price)),
      )
  }

trait SequenceK[U[f[_]]] {
  type Composed[F[_], G[_], A] = F[G[A]]

  def sequenceK[F[_]: Applicative, G[_]](uf: U[Composed[F, G, *]]): F[U[G]]

  def sequenceIdK[F[_]: Applicative](uf: U[F]): F[U[Id]] = sequenceK[F, Id](uf)
}

implicit val productSequence: SequenceK[Product] =
  new SequenceK[Product] {
    def sequenceK[F[_]: Applicative, G[_]](uf: Product[Composed[F, G, *]]): F[Product[G]] =
      (uf.name, uf.category, uf.price).mapN(Product.apply[G])
  }

trait TraverseK[U[f[_]]] extends RepresentableK[U] with SequenceK[U] {
  def traverseK[F[_]: Applicative, G[_]](fr: FunctionK[RepK[U, *], Composed[F, G, *]]): F[U[G]]

  def tabulate[F[_]](hom: FunctionK[RepK[U, *], F]): U[F] =
    traverseK[Id, F](hom)

  def sequenceK[F[_]: Applicative, G[_]](uf: U[Composed[F, G, *]]): F[U[G]] =
    traverseK[F, G](funK(rep => rep(uf)))
}

implicit val productTraverse: TraverseK[Product] =
  new TraverseK[Product] {
    def traverseK[F[_]: Applicative, G[_]](fr: FunctionK[RepK[Product, *], Composed[F, G, *]]): F[Product[G]] =
      (
        fr(RepK[Product](_.name)),
        fr(RepK[Product](_.category)),
        fr(RepK[Product](_.price))
      ).mapN(Product.apply[G])
  }

case class FieldInfo[A](name: String, annotations: Vector[AnyRef] = Vector.empty)

type HKDInfo[U[f[_]]] = U[FieldInfo]

implicit val productInfo: HKDInfo[Product] =
  Product[Lambda[A => String]](
    name = "name",
    category = "category",
    price = "price"
  ).mapK(funK(s => FieldInfo(s)))

trait HKDDerivation[TC[u[f[_]]], Field[_]] {
  def deriveWith[U[f[_]]](instances: => U[Field])(implicit traverse: TraverseK[U], info: HKDInfo[U]): TC[U]
}

def deriveInstance[U[f[_]], TC[u[f[_]]], Field[_]](derivator: HKDDerivation[TC, Field]): TC[U] = ??? // macro ...

trait HKDIdShow[U[f[_]]] {
  def encode(uid: U[Id]): String
}

def deriveInstanceShowProduct(derivator: HKDDerivation[HKDIdShow, Show]): HKDIdShow[Product] =
  derivator.deriveWith(
    Product[Show](
      name = implicitly,
      category = implicitly,
      price = implicitly
    )
  )

trait WrapInstance[F[_], TC[_]] {
  def wrap[A: TC]: TC[F[A]]
}

type EncoderK[F[_]] = WrapInstance[F, Encoder]
type DecoderK[F[_]] = WrapInstance[F, Encoder]

trait WrapHKD[U[f[_]], TC[_]] {
  implicit def wrapHKD[F[_]](implicit wrap: WrapInstance[F, TC]): TC[U[F]]
}

type HKDEncoder[U[f[_]]] = WrapHKD[U, Encoder]

type HKDDecoder[U[f[_]]] = WrapHKD[U, Decoder]

implicit val hkdEncoderDerive: HKDDerivation[HKDEncoder, Encoder] =
  new HKDDerivation[HKDEncoder, Encoder] {
    def deriveWith[U[f[_]]](
        instances: => U[Encoder]
    )(implicit traverse: TraverseK[U], info: HKDInfo[U]): HKDEncoder[U] =
      new HKDEncoder[U] {
        implicit def wrapHKD[F[_]](implicit wrap: WrapInstance[F, Encoder]): Encoder[U[F]] =
          new Encoder[U[F]] {
            def apply(data: U[F]): Json = {
              val jsons: U[Lambda[A => Json]]                   =
                data.zipWithK(instances)(Function2K.apply { (field, instance) =>
                  wrap.wrap(instance).apply(field)
                })
              val withName: U[Const[Vector[(String, Json)], *]] =
                jsons.zipWithK(info)(Function2K.apply { (json, finfo) =>
                  Const(Vector(finfo.name -> json))
                })

              Json.obj(traverse.sequenceIdK(withName).getConst: _*)
            }
          }
      }
  }

implicit val hkdDecoderDerive: HKDDerivation[HKDDecoder, Decoder] =
  new HKDDerivation[HKDDecoder, Decoder] {
    def deriveWith[U[f[_]]](
        instances: => U[Decoder]
    )(implicit traverse: TraverseK[U], info: HKDInfo[U]): HKDDecoder[U] =
      new HKDDecoder[U] {
        implicit def wrapHKD[F[_]](implicit wrap: WrapInstance[F, Decoder]): Decoder[U[F]] =
          new Decoder[U[F]] {
            def apply(c: HCursor): Decoder.Result[U[F]] = {
              val results: U[Lambda[A => Decoder.Result[F[A]]]] =
                info.zipWithK(instances)(Function2K { (finfo, instance) =>
                  c.downField(finfo.name).as(wrap.wrap(instance))
                })
              traverse.sequenceK[Decoder.Result, F](results)
            }
          }
      }
  }
