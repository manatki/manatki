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

  sealed trait CalcRes[-R, -S1, +S2, +E, +A] extends Calc[R, S1, S2, E, A] {
    def submit[X](r: R, s: S1, ke: (S2, E) => X, ka: (S2, A) => X): X
  }
  final case class Pure[S, +A](a: A) extends CalcRes[Any, S, S, Nothing, A] {
    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, A) => X): X = ka(s, a)
  }
  final case class Read[S, R]() extends CalcRes[R, S, S, Nothing, R] {
    def submit[X](r: R, s: S, ke: (S, Nothing) => X, ka: (S, R) => X): X = ka(s, r)
  }
  final case class Get[S]() extends CalcRes[Any, S, S, Nothing, S] {
    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, S) => X): X = ka(s, s)
  }
  final case class Set[S](s: S) extends CalcRes[Any, Any, S, Nothing, Unit] {
    def submit[X](r: Any, s1: Any, ke: (S, Nothing) => X, ka: (S, Unit) => X): X = ka(s, ())
  }
  final case class Raise[S, E](e: E) extends CalcRes[Any, S, S, E, Nothing] {
    def submit[X](r: Any, s: S, ke: (S, E) => X, ka: (S, Nothing) => X): X = ke(s, e)
  }
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
      case res: CalcRes[R, S1, S2, E, A] =>
        res.submit(r, init, (s2, e) => (s2, Left(e)), (s2, a) => (s2, Right(a)))
      case Defer(f) => run(f(), r, init)
      case c @ Cont(src, ks, ke) =>
        src match {
          case res: CalcRes[R, S1, c.MidState, c.MidErr, c.MidState] =>
            val (sm, next) =
              res.submit[(c.MidState, Calc[R, c.MidState, S2, E, A])](
                r,
                init,
                (sm, e) => (sm, ke(e)),
                (sm, a) => (sm, ks(a))
              )
            run(next, r, sm)
          case Defer(f) => run(f().cont(ks, ke), r, init)
          case c2 @ Cont(src1, ks1, ke1) =>
            run(src1.cont(a => ks1(a).cont(ks, ke), e => ke1(e).cont(ks, ke)), r, init)
        }
    }

  implicit def calcInstance[R, S, E]: CalcFunctorInstance[R, S, E] = new CalcFunctorInstance[R, S, E]

  class CalcFunctorInstance[R, S, E]
      extends MonadError[Calc[R, S, S, E, ?], E] with cats.Defer[Calc[R, S, S, E, ?]]
      with StackSafeMonad[Calc[R, S, S, E, ?]] {
    def defer[A](fa: => Calc[R, S, S, E, A]): Calc[R, S, S, E, A]                                     = Calc.defer(fa)
    def raiseError[A](e: E): Calc[R, S, S, E, A]                                                      = Calc.raise(e)
    def handleErrorWith[A](fa: Calc[R, S, S, E, A])(f: E => Calc[R, S, S, E, A]): Calc[R, S, S, E, A] = fa.handleWith(f)
    def flatMap[A, B](fa: Calc[R, S, S, E, A])(f: A => Calc[R, S, S, E, B]): Calc[R, S, S, E, B]      = fa.flatMap(f)
    def pure[A](x: A): Calc[R, S, S, E, A]                                                            = Calc.pure(x)
  }
}
