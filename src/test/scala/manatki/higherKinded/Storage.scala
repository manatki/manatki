package manatki.higherKinded
import cats.~>
import tofu.higherKind.{RepK, RepresentableK}

trait Storage[F[_], K, V] {
  def get(key: K): F[Option[V]]
  def put(key: K, value: V): F[Unit]
  def delete(key: K): F[Unit]
}

object Storage {
  implicit def representableK[K, V]: RepresentableK[Storage[*[_], K, V]] =
    new RepresentableK[Storage[*[_], K, V]] {
      type T[F[_]] = Storage[F, K, V]
      def tabulate[F[_]](hom: RepK[T, *] ~> F): T[F] =
        new Storage[F, K, V] {
          def get(key: K): F[Option[V]]      = hom(RepK[T](_.get(key)))
          def put(key: K, value: V): F[Unit] = hom(RepK[T](_.put(key, value)))
          def delete(key: K): F[Unit]        = hom(RepK[T](_.delete(key)))
        }
    }
}
