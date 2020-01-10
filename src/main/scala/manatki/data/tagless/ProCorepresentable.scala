package manatki.data.tagless
import cats.arrow.Profunctor
import cats.{Applicative, Functor, Traverse}

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

trait ProCorepresentable[P[_, _]] extends Profunctor[P] with Functor[Rep.Pro[P, *]] {
  def tabulate[A, B](k: Rep[P[A, *]] => B): P[A, B]

  def leftMap[A, B, C](fab: P[A, B])(f: C => A): P[C, B]

  override def lmap[A, B, C](fab: P[A, B])(f: C => A): P[C, B] = leftMap(fab)(f)
  override def rmap[A, B, C](fab: P[A, B])(f: B => C): P[A, C] = tabulate(rep => f(rep(fab)))

  def dimap[A, B, C, D](fab: P[A, B])(f: C => A)(g: B => D): P[C, D] = rmap(leftMap(fab)(f))(g)
  override def map[A, B](fa: Rep[P[A, *]])(f: A => B): Rep[P[B, *]] =
    fa(leftMap(tabulate(identity[Rep[P[B, *]]]))(f))

  def zip[A, B, C, D](pab: P[A, B], pcd: P[C, D]): P[(A, C), (B, D)] =
    tabulate(rep => (rep(leftMap(pab)(_._1)), rep(leftMap(pcd)(_._2))))
}

object ProCorepresentable {
  def tabulate[P[_, _], A, B](k: Rep[P[A, *]] => B)(implicit P: ProCorepresentable[P]): P[A, B] = P.tabulate(k)

  def construct[P[-_, +_]: ProCorepresentable]: P[Layer[P], Layer[P]] = tabulate[P, Layer[P], Layer[P]](_(construct))

  def constant[P[_, _]: ProCorepresentable, A, B](b: B): P[A, B] = tabulate[P, A, B](_ => b)

  def zip[P[_, _], A, B, C, D](pab: P[A, B], pcd: P[C, D])(implicit P: ProCorepresentable[P]): P[(A, C), (B, D)] =
    P.zip(pab, pcd)

  class Tab[A, B, P[_, _]](val k: Rep[P[A, *]] => B)

  class LMap[A, B, C, P[_, _]](val pab: P[A, B], val f: C => A)
}

trait ProTraverse[P[_, _]] extends ProCorepresentable[P] {
  def protraverse[F[_]: Applicative, A, B](p: P[A, B]): P[F[A], F[B]]
}

object ProTraverse {
  trait ByTraverse[P[_, _]] extends ProTraverse[P] with Traverse[Rep.Pro[P, *]] {
    def protraverse[F[_], A, B](p: P[A, B])(implicit F: Applicative[F]): P[F[A], F[B]] =
      tabulate(rep => F.map(sequence[F, A](rep))(_(p)))
  }

  class ProTrav[F[_], A, B, P[_, _]](val pab: P[A, B])(implicit val F: Applicative[F])
}