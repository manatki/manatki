package manatki.data.tagless
import cats.{Applicative, Functor, Id, Traverse}
import manatki.data.tagless.ProTraverse.Tab
import manatki.data.tagless.ProTraverseMakers.TabPure
import simulacrum.typeclass

import scala.annotation.unchecked.{uncheckedVariance => uv}

trait Rep[-P[_, _], A] {
  def apply[R](fa: P[A, R]): R
}

trait PBase[P[_, _]]
object PBase {
  import tofu.syntax.monadic._
  type Extend1[X1, P[-i, +o], -I, +O]     = P[I, X1 => O]
  type Extend2[X1, X2, P[-i, +o], -I, +O] = P[I, (X1, X2) => O]

  final implicit def extend1[X1, P[-i, +o]](implicit pt: ProTraverse[P]): ProTraverse[Extend1[X1, P, -*, +*]] =
    new ProTraverse[Extend1[X1, P, -*, +*]] {
      def tabTraverse[F[_]: Applicative, A, B, C](
          left: A => F[B]
      )(right: F[Rep[Extend1[X1, P, -*, +*], B]] => C): P[A, X1 => C] =
        pt.tabTraverse[F, A, B, X1 => C](left)(fpr =>
          x1 => right(fpr.map(r => Rep[Extend1[X1, P, -*, +*], B](p => r(p)(x1))))
        )
    }

  final implicit def extend2[X1, X2, P[-i, +o]](implicit pt: ProTraverse[P]): ProTraverse[Extend2[X1, X2, P, -*, +*]] =
    new ProTraverse[Extend2[X1, X2, P, -*, +*]] {
      def tabTraverse[F[_]: Applicative, A, B, C](
          left: A => F[B]
      )(right: F[PR[B]] => C): P[A, (X1, X2) => C] =
        pt.tabTraverse[F, A, B, (X1, X2) => C](left)(fpr =>
          (x1, x2) => right(fpr.map(r => Rep[Extend2[X1, X2, P, -*, +*], B](p => r(p)(x1, x2))))
        )
    }
}

trait LMap[P[_, _]] extends PBase[P] {
  def lmap[A, B, C](fab: P[A, B])(f: C => A): P[C, B]
}

@typeclass trait RMap[P[_, _]] extends PBase[P] {
  def rmap[A, B, C](fab: P[A, B])(f: B => C): P[A, C]

  final def flipOut[A, X1, R](f: P[A, X1 => R])(x1: X1): P[A, R]                    = rmap(f)(_(x1))
  final def flipOut2[A, X1, X2, R](f: P[A, (X1, X2) => R])(x1: X1, x2: X2): P[A, R] = rmap(f)(_(x1, x2))
}

trait Pro[P[_, _]] extends LMap[P] with RMap[P] { self =>
  def dimap[A, B, C, D](fab: P[A, B])(f: C => A)(g: B => D): P[C, D] = rmap(lmap(fab)(f))(g)
}

object Rep {
  def apply[P[-_, _], A] = new Applied[P, A](true)
  def mk[P[-_, _], A]    = new Applied[P, A](true)

  class Applied[P[-_, _], A](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(maker: MakeRepr[P, A, Arb]): Rep[P, A] = maker
  }

  abstract class MakeRepr[P[-_, _], A, Arb] extends Rep[P, A] {
    def applyArbitrary(fk: P[A, Arb]): Arb

    def apply[R](fk: P[A, R]): R = applyArbitrary(fk.asInstanceOf[P[A, Arb]]).asInstanceOf[R]
  }

  implicit def prorepFunctor[P[-_, _]](implicit P: Pro[P]): Functor[Rep[P, *]] = new Functor[Rep[P, *]] {
    def map[A, B](fa: Rep[P, A])(f: A => B): Rep[P, B] = Rep[P, B](p => fa(P.lmap(p)(f)))
  }

  implicit class ProfRepOps[P[-_, _], A](private val self: Rep[P, A]) extends AnyVal {
    def pmap[B](f: A => B)(implicit P: Pro[P]): Rep[P, B] = Rep[P, B](p => self(P.lmap(p)(f)))
  }
}

trait ProCorep[P[_, _]] extends Pro[P] {
  type PR[A] = Rep[P, A]

  def cotabulate[A, B](k: Rep[P, A] => B): P[A, B]

  override def rmap[A, B, C](fab: P[A, B])(f: B => C): P[A, C] = cotabulate(rep => f(rep(fab)))

  def zip[A, B, C, D](pab: P[A, B], pcd: P[C, D]): P[(A, C), (B, D)] =
    cotabulate(rep => (rep(lmap(pab)(_._1)), rep(lmap(pcd)(_._2))))

  def merge[A, B, C](pab: P[A, B], pac: P[A, C]): P[A, (B, C)] =
    cotabulate(rep => (rep(pab), rep(pac)))

  def functor: Functor[Rep[P, *]] = new Functor[Rep[P, *]] {
    def map[A, B](fa: Rep[P, A])(f: A => B): Rep[P, B] = fa(lmap(cotabulate[B, Rep[P, B]](identity[Rep[P, B]]))(f))
  }

  def constant[A, B](b: B): P[A, B] = cotabulate(_ => b)

  def construct[Q[-x, y] <: P[x, y] @uv]: P[Layer[Q], Layer[Q]] = cotabulate(f => Layer[Q](f(_)))
}

object ProCorep {
  def construct[P[-_, _]](implicit P: ProCorep[P]): P[Layer[P], Layer[P]] = P.construct

  implicit class Ops[A, B, P[_, _]](private val self: P[A, B]) extends AnyVal {
    def zip[C, D](that: P[C, D])(implicit P: ProCorep[P]): P[(A, C), (B, D)] = P.zip(self, that)
    def merge[C](that: P[A, C])(implicit P: ProCorep[P]): P[A, (B, C)]       = P.merge(self, that)
  }
}

trait ProTraverse[P[_, _]] extends ProCorep[P] {
  def prosequence[F[_], A, B](p: P[A, B])(implicit F: Applicative[F]): P[F[A], F[B]] =
    tabTraverse[F, F[A], A, F[B]](identity)(F.map(_)(_(p)))

  override def cotabulate[A, B](k: Rep[P, A] => B): P[A, B] =
    tabTraverse[Id, A, A, B](identity)(k)

  override def lmap[A, B, C](fab: P[A, B])(f: C => A): P[C, B] =
    tabTraverse[Id, C, A, B](f)(_(fab))

  def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): P[A, C]
}

object ProTraverse {

  class Tab[F[_], A, B, C, P[-_, _]](val left: A => F[B], val right: F[Rep[P, B]] => C)(implicit
      val F: Applicative[F]
  ) {
    import ProTraverseMakers._
    final def rep                                                                           = Rep[P, B]
    final def tabPure(rep: Rep[P, B]): C                                                    = right(F.pure(rep))
    final def tabMap(a: A)(f: B => Rep[P, B]): C                                            = right(F.map(left(a))(f))
    final def tabMap2(x: A, y: A)(f: (B, B) => Rep[P, B]): C                                = right(F.map2(left(x), left(y))(f))
    final def tabTraverse[T[_]](as: T[A])(f: T[B] => Rep[P, B])(implicit T: Traverse[T]): C =
      right(F.map(T.traverse(as)(left))(f))
    final def mkPure                                                                        = new TabPure(this)
    final def mkMap                                                                         = new TabMap(this)
    final def mkMap2                                                                        = new TabMap2(this)
    final def mkTraverse                                                                    = new TabTrav(this)
  }

  def make[P[-_, _]] = new Maker[P]

  class Maker[P[-_, _]] {
    type Fa[_]
    type Aa
    type Ba
    type Ca
    def apply(instance: Applied[Fa, Aa, Ba, Ca, P]): ProTraverse[P] = instance
  }

  abstract class Applied[Fa[_], Aa, Ba, Ca, P[-_, _]] extends ProTraverse[P] {
    def tabTravArb(left: Aa => Fa[Ba])(right: Fa[Rep[P, Ba]] => Ca)(F: Applicative[Fa]): P[Aa, Ca]

    override def tabTraverse[F[_], A, B, C](left: A => F[B])(right: F[Rep[P, B]] => C)(implicit
        F: Applicative[F]
    ): P[A, C] =
      tabTravArb(left.asInstanceOf[Aa => Fa[Ba]])(right.asInstanceOf[Fa[Rep[P, Ba]] => Ca])(
        F.asInstanceOf[Applicative[Fa]]
      ).asInstanceOf[P[A, C]]
  }
}

object ProTraverseMakers {
  import Rep.MakeRepr
  trait HasArb extends Any {
    type Arb
  }

  class TabPure[P[-_, _], A, B, C, F[_]](private val tab: Tab[F, A, B, C, P]) extends AnyVal with HasArb {
    def apply(f: MakeRepr[P, B, Arb]): C = tab.tabPure(f)
  }

  class TabMap[P[-_, _], A, B, C, F[_]](private val tab: Tab[F, A, B, C, P]) extends AnyVal with HasArb {
    def apply(a: A)(f: B => MakeRepr[P, B, Arb]): C = tab.tabMap(a)(f)
  }

  class TabMap2[P[-_, _], A, B, C, F[_]](private val tab: Tab[F, A, B, C, P]) extends AnyVal with HasArb {
    def apply(x: A, y: A)(f: (B, B) => MakeRepr[P, B, Arb]): C = tab.tabMap2(x, y)(f)
  }

  class TabTrav[P[-_, _], A, B, C, F[_]](private val tab: Tab[F, A, B, C, P]) extends AnyVal with HasArb {
    def apply[T[_]: Traverse](xs: T[A])(f: T[B] => MakeRepr[P, B, Arb]): C = tab.tabTraverse(xs)(f)
  }
}
