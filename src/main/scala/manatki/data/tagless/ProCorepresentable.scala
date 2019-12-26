package manatki.data.tagless
import cats.arrow.Profunctor
import cats.{Contravariant, Functor}

trait Rep[F[_]] {
  def apply[A](fa: F[A]): A
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

    def apply[R](fk: T[R]): R = applyArbitrary(fk.asInstanceOf[T[Arb]]).asInstanceOf[R]
  }
}

trait Representable[F[_]] {
  def tabulate[A](fa: Rep[F] => A): F[A]
}

object Representable {
  def index[F[_], A](fa: F[A], r: Rep[F]): A = r.apply(fa)

}

trait ListP[I, A, B] {
  def nil: B
  def cons(head: I, tail: A): B
}

object ListP {
  implicit def corepresentable[I]: ProCorepresentable[ListP[I, *, *]] =
    new ProCorepresentable[ListP[I, *, *]] {
      def tabulate[A, B](k: Rep[ListP[I, A, *]] => B): ListP[I, A, B] =
        new ListP[I, A, B] {
          def nil: B                    = k(Rep.mk(_.nil))
          def cons(head: I, tail: A): B = k(Rep.mk(_.cons(head, tail)))
        }

      def leftMap[A, B, C](fab: ListP[I, A, B])(f: C => A): ListP[I, C, B] =
        new ListP[I, C, B] {
          def nil: B                    = fab.nil
          def cons(head: I, tail: C): B = fab.cons(head, f(tail))
        }

    }
}

trait ProCorepresentable[P[_, _]] extends Profunctor[P] with Functor[λ[A => Rep[P[A, *]]]] {
  def tabulate[A, B](k: Rep[P[A, *]] => B): P[A, B]

  def leftMap[A, B, C](fab: P[A, B])(f: C => A): P[C, B]

  override def lmap[A, B, C](fab: P[A, B])(f: C => A): P[C, B] = leftMap(fab)(f)
  override def rmap[A, B, C](fab: P[A, B])(f: B => C): P[A, C] = tabulate(rep => f(rep(fab)))

  def dimap[A, B, C, D](fab: P[A, B])(f: C => A)(g: B => D): P[C, D] = rmap(leftMap(fab)(f))(g)
  override def map[A, B](fa: Rep[P[A, *]])(f: A => B): Rep[P[B, *]] =
    fa(leftMap(tabulate(identity[Rep[P[B, *]]]))(f))
}

object ProCorepresentable {
  def tabulate[P[_, _], A, B](k: Rep[P[A, *]] => B)(implicit P: ProCorepresentable[P]): P[A, B] =
    P.tabulate(k)

  def construct[P[-_, +_]: ProCorepresentable]: P[Layer[P], Layer[P]] =
    tabulate[P, Layer[P], Layer[P]](_(construct))
}
