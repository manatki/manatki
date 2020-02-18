package manatki.data.tagless
import manatki.data.tagless.FunK2.MakeFunctionK2

trait FunK2[-P[_, _], +Q[_, _]] {
  def apply[A, B](p: P[A, B]): Q[A, B]
}

object FunK2 {
  def apply[P[_, _], Q[_, _]](maker: MakeFunctionK2[P, Q]): FunK2[P, Q] = maker
  abstract class MakeFunctionK2[-P[_, _], +Q[_, _]] extends FunK2[P, Q] {
    def applyArbitrary(f: P[Arb1, Arb2]): Q[Arb1, Arb2]

    def apply[A, B](p: P[A, B]): Q[A, B] =
      applyArbitrary(p.asInstanceOf[P[Arb1, Arb2]]).asInstanceOf[Q[A, B]]
  }
  type Arb1
  type Arb2
}

trait IsoK2[P[_, _], Q[_, _]] extends FunK2[P, Q] { self =>
  def back[A, B](q: Q[A, B]): P[A, B]
  def invert: IsoK2[Q, P] = IsoK2.invert(this)
}

object IsoK2 {
  def invert[P[_, _], Q[_, _]](self: IsoK2[P, Q]): IsoK2[Q, P] = new IsoK2[Q, P] {
    def back[A, B](p: P[A, B]): Q[A, B]  = self(p)
    def apply[A, B](q: Q[A, B]): P[A, B] = self.back(q)
    override val invert: IsoK2[P, Q]     = self
  }

  def apply[P[_, _], Q[_, _]](makeTo: MakeFunctionK2[P, Q])(makeFrom: MakeFunctionK2[Q, P]): FunK2[P, Q] =
    new IsoK2[P, Q] {
      def back[A, B](q: Q[A, B]): P[A, B]  = makeFrom(q)
      def apply[A, B](p: P[A, B]): Q[A, B] = makeTo(p)
    }

}
