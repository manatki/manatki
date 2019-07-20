package manatki.control
import cats.{Functor, Monad}
import cats.data.{OptionT, Tuple2K}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.tagless.ApplyK
import cats.tagless.syntax.semigroupalK._
import cats.tagless.syntax.functorK._
import manatki.control.Caching.{Delete, Get, Put}
import cats.syntax.flatMap._
import manatki.syntax.functionK
import cats.syntax.applicative._
import cats.syntax.functor._

trait Caching[T[_[_]], F[_]] {
  def put: T[Put[F, *]]
  def read: T[Get[F, *]]
  def delete: T[Delete[F, *]]
}

object Caching {

  type Delete[F[_], A] = F[Unit]
  type Put[F[_], A]    = A => F[Unit]
  type Get[F[_], A]    = F[Option[A]]

  def cached[T[_[_]]: ApplyK, F[_]: Monad](instance: T[F], caching: Caching[T, F]): T[F] =
    instance
      .productK[Put[F, *]](caching.put)
      .productK[Get[F, *]](caching.read)
      .mapK(
        functionK.apply(ttk => OptionT(ttk.second).getOrElseF(ttk.first.first.flatTap(ttk.first.second)))
      )
}

trait ReadFooService[F[_]] {
  def getFoo(name: String): F[Long]
  def getBar(id: Long, fooName: String): F[String]
}

object ReadFooService {
  implicit val applyK: ApplyK[ReadFooService] = cats.tagless.Derive.applyK

  // Should be generated with macro
  def caching[F[_]: Sync]: F[Caching[ReadFooService, F]] =
    for {
      getFooRef <- Ref[F].of(Map.empty[String, Long])
      getBarRef <- Ref[F].of(Map.empty[(Long, String), String])
    } yield
      new Caching[ReadFooService, F] {
        val put    = new ReadFooServicePut(getFooRef, getBarRef)
        val read   = new ReadFooServiceGet(getFooRef, getBarRef)
        val delete = new ReadFooServiceDelete(getFooRef, getBarRef)
      }

  class ReadFooServicePut[F[_]](getFooRef: Ref[F, Map[String, Long]], getBarRef: Ref[F, Map[(Long, String), String]])
      extends ReadFooService[Put[F, ?]] {
    def getFoo(name: String): Long => F[Unit]                = res => getFooRef.update(_ + ((name, res)))
    def getBar(id: Long, fooName: String): String => F[Unit] = res => getBarRef.update(_ + (((id, fooName), res)))
  }

  class ReadFooServiceDelete[F[_]](getFooRef: Ref[F, Map[String, Long]], getBarRef: Ref[F, Map[(Long, String), String]])
      extends ReadFooService[Delete[F, ?]] {
    def getFoo(name: String): F[Unit]              = getFooRef.update(_ - name)
    def getBar(id: Long, fooName: String): F[Unit] = getBarRef.update(_ - ((id, fooName)))
  }

  class ReadFooServiceGet[F[_]: Functor](getFooRef: Ref[F, Map[String, Long]],
                                         getBarRef: Ref[F, Map[(Long, String), String]])
      extends ReadFooService[Get[F, ?]] {
    def getFoo(name: String): F[Option[Long]]                = getFooRef.get.map(_.get(name))
    def getBar(id: Long, fooName: String): F[Option[String]] = getBarRef.get.map(_.get((id, fooName)))
  }
}
