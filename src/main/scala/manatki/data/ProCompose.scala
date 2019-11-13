package manatki.data
import cats.arrow.{Category, Profunctor}
import cats.syntax.profunctor._

sealed trait ProCompose[P[_, _], Q[_, _], A, B] {
  type Mid
  def pam: P[A, Mid]
  def qmb: Q[Mid, B]
}

object ProCompose {
  def apply[P[_, _], Q[_, _], A, M, B](pam: P[A, M], qmb: Q[M, B]): ProCompose[P, Q, A, B] = Impl(pam, qmb)

  final case class Impl[P[_, _], Q[_, _], A, M, B](pam: P[A, M], qmb: Q[M, B]) extends ProCompose[P, Q, A, B] {
    type Mid = M
  }

  implicit def procomposeProfunctor[P[_, _]: Profunctor, Q[_, _]: Profunctor]: Profunctor[ProCompose[P, Q, *, *]] =
    new Profunctor[ProCompose[P, Q, *, *]] {
      def dimap[A, B, C, D](fab: ProCompose[P, Q, A, B])(f: C => A)(g: B => D): ProCompose[P, Q, C, D] =
        ProCompose(fab.pam.lmap(f), fab.qmb.rmap(g))
    }
}

trait ProTrans[P[_, _], Q[_, _]] { self =>
  def apply[A, B](pab: P[A, B]): Q[A, B]

  def compose[T[_, _]](t: ProTrans[T, P]): ProTrans[T, Q] = new ProTrans[T, Q] {
    def apply[A, B](pab: T[A, B]): Q[A, B] = self(t(pab))
  }

  def hcomp[S[_, _], T[_, _]](t: ProTrans[S, T]): ProTrans[ProCompose[P, S, *, *], ProCompose[Q, T, *, *]] =
    new ProTrans[ProCompose[P, S, *, *], ProCompose[Q, T, *, *]] {
      def apply[A, B](pab: ProCompose[P, S, A, B]): ProCompose[Q, T, A, B] =
        ProCompose(self(pab.pam), t(pab.qmb))
    }
}

object ProTrans {
  def id[P[_, _]]: ProTrans[P, P] = new ProTrans[P, P] {
    def apply[A, B](pab: P[A, B]): P[A, B] = pab
  }
}

trait ProMonad[P[_, _]] extends Category[P] { pm =>
  def compose[A, B, C](pbc: P[B, C], pab: P[A, B]): P[A, C]
  def lift[A, B](f: A => B): P[A, B]
  def id[A]: P[A, A] = lift(identity)

  def flatten: ProTrans[ProCompose[P, P, *, *], P] = new ProTrans[ProCompose[P, P, *, *], P] {
    def apply[A, B](pp: ProCompose[P, P, A, B]): P[A, B] = pm.compose(pp.qmb, pp.pam)
  }
  def unit: ProTrans[* => *, P] = new ProTrans[* => *, P] {
    def apply[A, B](f: A => B): P[A, B] = pm.lift(f)
  }
}
