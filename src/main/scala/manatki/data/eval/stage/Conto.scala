package manatki.data.eval.stage
import manatki.data.eval.stage.Conto.FlatMap

sealed trait Conto[R, A] {
  def map[B](f: A => B): Conto[R, B] = FlatMap[R, A, B](this, a => Conto.Pure(f(a)))
}

object Conto {
  case class Pure[R, A](a: A)                                       extends Conto[R, A]
  case class FlatMap[R, A, B](fa: Conto[R, A], f: A => Conto[R, B]) extends Conto[R, B]
  trait Cn[R, A] extends Conto[R, A] {
    def cont(kc: A => Conto[R, R]): Conto[R, R]
  }

  def loop[R, A](conto: Conto[R, A])(k: A => R): R =
    conto match {
      case Pure(a)     => k(a)
      case u: Cn[R, A] => loop(u.cont(a => Pure(k(a))))(identity)
      case fm: FlatMap[R, x, A] =>
        type X = x
        fm.fa match {
          case Pure(b)               => loop(fm.f(b))(k)
          case fm2: FlatMap[R, y, X] => loop(FlatMap[R, y, A](fm2.fa, y => FlatMap[R, x, A](fm2.f(y), fm.f)))(k)
          case u: Cn[R, X]           => loop(u.cont(x => fm.f(x).map(k)))(identity)
        }
    }

}
