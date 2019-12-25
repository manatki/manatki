package manatki.data.tagless
import cats.{Contravariant, Functor}
import scalaz.Profunctor

trait Rep[F[_]] {
  def cont[A](fa: F[A]): A
}

object Rep {
  type Pro[P[a, b], A] = Rep[P[A, *]]

  def apply[U[_]] = new Applied[U](true)
  def mk[U[_]]    = new Applied[U](true)

  class Applied[T[_]](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(maker: MakeRepr[T, Arb]): Rep[T] = maker
  }

  abstract class MakeRepr[T[_], Arb] extends Rep[T] {
    def applyArbitrary(fk: T[Arb]): Arb

    def cont[R](fk: T[R]): R = applyArbitrary(fk.asInstanceOf[T[Arb]]).asInstanceOf[R]
  }
}

trait Representable[F[_]] {
  def tabulate[A](fa: Rep[F] => A): F[A]
}

object Representable {
  def index[F[_], A](fa: F[A], r: Rep[F]): A = r.cont(fa)

}

trait ListP[I, A, B] {
  def nil: B
  def cons(head: I, tail: A): B
}

object ListP {
  implicit def corepresentable[I]: CorepresentablePro[ListP[I, *, *]] =
    new CorepresentablePro[ListP[I, *, *]] {
      def tabulate[A, B](k: Rep[ListP[I, A, *]] => B): ListP[I, A, B] =
        new ListP[I, A, B] {
          def nil: B                    = k(Rep.mk(_.nil))
          def cons(head: I, tail: A): B = k(Rep.mk(_.cons(head, tail)))
        }

      def mapfst[A, B, C](fab: ListP[I, A, B])(f: C => A): ListP[I, C, B] =
        new ListP[I, C, B] {
          def nil: B                    = fab.nil
          def cons(head: I, tail: C): B = fab.cons(head, f(tail))
        }
    }
}

trait CorepresentablePro[P[_, _]] extends Profunctor[P] {
  def tabulate[A, B](k: Rep[P[A, *]] => B): P[A, B]
  def mapsnd[A, B, C](fab: P[A, B])(f: B => C): P[A, C] = tabulate(rep => f(rep.cont(fab)))
}
