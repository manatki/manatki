package manatki.data.eval

import cats.{MonadError, StackSafeMonad}
import cats.kernel.Monoid
import cats.syntax.either._

sealed trait Calc[-R, -S1, +S2, +E, +A] {
  final def run(r: R, init: S1): (S2, Either[E, A])    = Calc.run(this, r, init)
  final def runUnit(init: S1)(implicit ev: Unit <:< R) = run((), init)
}

object Calc {
  def pure[S, A](a: A): Calc[Any, S, S, Nothing, A]  = Pure(a)
  def read[S, R]: Calc[R, S, S, Nothing, R]          = Read()
  def get[S]: Calc[Any, S, S, Nothing, S]            = Get()
  def set[S](s: S): Calc[Any, Any, S, Nothing, Unit] = Set(s)
  def update[S1, S2](f: S1 => S2): Calc[Any, S1, S2, Nothing, Unit] =
    get[S1].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): Calc[Any, S, S, E, Nothing]      = Raise(e)
  def defer[R, S1, S2, E, A](x: => Calc[R, S1, S2, E, A]) = Defer(() => x)
  def delay[S, A](x: => A): Calc[Any, S, S, Nothing, A]   = defer(pure(x))

  def write[S](s: S)(implicit S: Monoid[S]): Calc[Any, S, S, Nothing, Unit] = update(S.combine(_, s))

  final case class Pure[S, +A](a: A)                                      extends Calc[Any, S, S, Nothing, A]
  final case class Read[S, R]()                                          extends Calc[R, S, S, Nothing, R]
  final case class Get[S]()                                               extends Calc[Any, S, S, Nothing, S]
  final case class Set[S](s: S)                                           extends Calc[Any, Any, S, Nothing, Unit]
  final case class Raise[S, E](e: E)                                      extends Calc[Any, S, S, E, Nothing]
  final case class Defer[R, S1, S2, E, A](e: () => Calc[R, S1, S2, E, A]) extends Calc[R, S1, S2, E, A]
  final case class Cont[R, S1, S2, S3, E1, E2, A, B](
      src: Calc[R, S1, S2, E1, A],
      ksuc: A => Calc[R, S2, S3, E2, B],
      kerr: E1 => Calc[R, S2, S3, E2, B]
  ) extends Calc[R, S1, S3, E2, B] {
    type MidState = S2
    type MidErr   = E1
  }

  implicit class invariantOps[R, S1, S2, E, A](val calc: Calc[R, S1, S2, E, A]) extends AnyVal {
    def cont[E2, S3, B](f: A => Calc[R, S2, S3, E2, B], h: E => Calc[R, S2, S3, E2, B]): Calc[R, S1, S3, E2, B] =
      Cont(calc, f, h)
    def flatMap[B](f: A => Calc[R, S2, S2, E, B]): Calc[R, S1, S2, E, B] = cont(f, raise(_: E))
    def handleWith(f: E => Calc[R, S2, S2, E, A]): Calc[R, S1, S2, E, A] = cont(pure(_: A), f)
    def handle(f: E => A): Calc[R, S1, S2, E, A]                         = handleWith(e => pure(f(e)))
    def map[B](f: A => B): Calc[R, S1, S2, E, B]                         = flatMap(a => pure(f(a)))
  }

  implicit class successfullOps[R, S1, S2, A](val calc: Calc[R, S1, S2, Nothing, A]) extends AnyVal {
    def flatMapS[S3, B, E](f: A => Calc[R, S2, S3, E, B]): Calc[R, S1, S3, E, B] =
      calc.cont(f, (void: Nothing) => void)
  }

  def run[R, S1, S2, E, A](calc: Calc[R, S1, S2, E, A], r: R, init: S1): (S2, Either[E, A]) =
    calc match {
      case Pure(a)      => (init.asInstanceOf[S2], Right(a))
      case Read()       => (init.asInstanceOf[S2], Right(r.asInstanceOf[A]))
      case Get()        => (init.asInstanceOf[S2], Right(init.asInstanceOf[A]))
      case set: Set[S2] => (set.s, Right(().asInstanceOf[A]))
      case Raise(e)     => (init.asInstanceOf[S2], Left(e))
      case Defer(f)     => run(f(), r, init)
      case c @ Cont(src, ks, ke) =>
        val kee = ke.asInstanceOf[c.MidErr => Calc[R, c.MidState, S2, E, A]]
        src match {
          case Pure(a)      => run(ks(a), r, init)
          case Read()       => run(ks(r.asInstanceOf[A]), r, init)
          case Get()        => run(ks(init.asInstanceOf[A]), r, init)
          case set: Set[S2] => run(ks(().asInstanceOf[A]), r, set.s)
          case Raise(e)     => run(kee(e), r, init)
          case Defer(f)     => run(f().cont(ks, kee), r, init)
          case c2 @ Cont(src1, ks1, ke1) =>
            val kee1 = ke1.asInstanceOf[c2.MidErr => Calc[R, c2.MidState, c.MidState, E, A]]
            run(src1.cont(a => ks1(a).cont(ks, kee), e => kee1(e).cont(ks, kee)), r, init)
        }
    }

  implicit def calcInstance[R, S, E]: CalcFunctorInstance[R, S, E] = new CalcFunctorInstance[R, S, E]

  class CalcFunctorInstance[R, S, E]
      extends MonadError[Calc[R, S, S, E, ?], E] with cats.Defer[Calc[R, S, S, E, ?]] with StackSafeMonad[Calc[R, S, S, E, ?]] {
    def defer[A](fa: => Calc[R, S, S, E, A]): Calc[R, S, S, E, A]                                     = Calc.defer(fa)
    def raiseError[A](e: E): Calc[R, S, S, E, A]                                                      = Calc.raise(e)
    def handleErrorWith[A](fa: Calc[R, S, S, E, A])(f: E => Calc[R, S, S, E, A]): Calc[R, S, S, E, A] = fa.handleWith(f)
    def flatMap[A, B](fa: Calc[R, S, S, E, A])(f: A => Calc[R, S, S, E, B]): Calc[R, S, S, E, B]      = fa.flatMap(f)
    def pure[A](x: A): Calc[R, S, S, E, A]                                                            = Calc.pure(x)
  }
}
