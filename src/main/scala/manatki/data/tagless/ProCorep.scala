package manatki.data.tagless
import cats.arrow.Profunctor
import cats.{Applicative, Functor, Traverse}
import manatki.data.tagless.Rep.prof
import simulacrum.typeclass

trait Rep[-F[_]] {
  def apply[A](fa: F[A]): A
}

@typeclass
trait Pro[F[-_, +_]] { self =>
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] = rmap(lmap(fab)(f))(g)

  def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B]

  def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C]
}

object Rep {
  type prof[-P[a, b], A] = Rep[P[A, *]]

  def apply[U[_]]     = new Applied[U](true)
  def mk[U[_]]        = new Applied[U](true)
  def pro[P[_, _], A] = new Applied[P[A, *]](true)

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
@typeclass
trait ProCorep[P[-_, +_]] extends Pro[P] {
  def tabulate[A, B](k: Rep[P[A, *]] => B): P[A, B]

  override def rmap[A, B, C](fab: P[A, B])(f: B => C): P[A, C] = tabulate(rep => f(rep(fab)))

  def zip[A, B, C, D](pab: P[A, B], pcd: P[C, D]): P[(A, C), (B, D)] =
    tabulate(rep => (rep(lmap(pab)(_._1)), rep(lmap(pcd)(_._2))))

  def functor: Functor[Rep.prof[P, *]] = new Functor[prof[P, *]] {
    def map[A, B](fa: prof[P, A])(f: A => B): prof[P, B] = fa(lmap(tabulate(identity[Rep[P[B, *]]]))(f))
  }

  def construct: P[Layer[P], Layer[P]] = tabulate(_(construct))

  def constant[A, B](b: B): P[A, B] = tabulate(_ => b)
}

object ProCorep {
  class Tab[A, B, P[_, _]](val k: Rep[P[A, *]] => B)

  class LMap[A, B, C, P[_, _]](val pab: P[A, B], val f: C => A)
}

@typeclass
trait ProTraverse[P[-_, +_]] extends ProCorep[P] {
  def protraverse[F[_]: Applicative, A, B](p: P[A, B]): P[F[A], F[B]]
}

object ProTraverse {
  trait ByTraverse[P[-_, +_]] extends ProTraverse[P] with Traverse[Rep.prof[P, *]] {
    def protraverse[F[_], A, B](p: P[A, B])(implicit F: Applicative[F]): P[F[A], F[B]] =
      tabulate(rep => F.map(sequence[F, A](rep))(_(p)))
  }

  class ProTrav[F[_], A, B, P[-_, +_]](val pab: P[A, B])(implicit val F: Applicative[F])
}
