package manatki.data.eval.stage

import cats.{MonadError, StackSafeMonad}
import cats.kernel.Monoid
import cats.syntax.either._

sealed trait Calc[-R, -I, -S1, +S2, +O, +E, +A] {
  final def run(r: R, init: S1): (S2, Either[E, A])    = Calc.run(this, r, init)
  final def runUnit(init: S1)(implicit ev: Unit <:< R) = run((), init)
}

object Calc {
  def pure[S, A](a: A): Calc[Any, Any, S, S, Nothing, Nothing, A]  = Pure(a)
  def read[S, R]: Calc[R, Any, S, S, Nothing, Nothing, R]          = Read()
  def get[S]: Calc[Any, Any, S, S, Nothing, Nothing, S]            = Get()
  def set[S](s: S): Calc[Any, Any, Any, S, Nothing, Nothing, Unit] = Set(s)
  def update[S1, S2](f: S1 => S2): Calc[Any, Any, S1, S2, Nothing, Nothing, Unit] =
    get[S1].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): Calc[Any, Any, S, S, Nothing, E, Nothing]    = Raise(e)
  def defer[R, I, S1, S2, O, E, A](x: => Calc[R, I, S1, S2, O, E, A]) = Defer(() => x)
  def delay[S, A](x: => A): Calc[Any, Any, S, S, Nothing, Nothing, A] = defer(pure(x))

  def write[S](s: S)(implicit S: Monoid[S]): Calc[Any, Any, S, S, Nothing, Nothing, Unit] = update(S.combine(_, s))

  sealed trait CalcRes[-R, -I, -S1, +S2, +O, +E, +A] extends Calc[R, I, S1, S2, O, E, A] {
    def submit[X](r: R, s: S1, ke: (S2, E) => X, ka: (S2, A) => X): X
  }
  final case class Pure[S, +A](a: A) extends CalcRes[Any, Any, S, S, Nothing, Nothing, A] {
    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, A) => X): X = ka(s, a)
  }
  final case class Read[S, R]() extends CalcRes[R, Any, S, S, Nothing, Nothing, R] {
    def submit[X](r: R, s: S, ke: (S, Nothing) => X, ka: (S, R) => X): X = ka(s, r)
  }
  final case class Get[S]() extends CalcRes[Any, Any, S, S, Nothing, Nothing, S] {
    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, S) => X): X = ka(s, s)
  }
  final case class Set[S](s: S) extends CalcRes[Any, Any, Any, S, Nothing, Nothing, Unit] {
    def submit[X](r: Any, s1: Any, ke: (S, Nothing) => X, ka: (S, Unit) => X): X = ka(s, ())
  }
  final case class Raise[S, E](e: E) extends CalcRes[Any, Any, S, S, Nothing, E, Nothing] {
    def submit[X](r: Any, s: S, ke: (S, E) => X, ka: (S, Nothing) => X): X = ke(s, e)
  }
  final case class Defer[R, I, S1, S2, O, E, A](e: () => Calc[R, I, S1, S2, O, E, A])
      extends Calc[R, I, S1, S2, O, E, A]
  final case class Cont[R, I, S1, S2, S3, O, E1, E2, A, B](
      src: Calc[R, I, S1, S2, O, E1, A],
      ksuc: A => Calc[R, I, S2, S3, O, E2, B],
      kerr: E1 => Calc[R, I, S2, S3, O, E2, B]
  ) extends Calc[R, I, S1, S3, O, E2, B] {
    type MidState = S2
    type MidErr   = E1
  }

  implicit class invariantOps[R, I, S1, S2, O, E, A](val calc: Calc[R, I, S1, S2, O, E, A]) extends AnyVal {
    def cont[E2, S3, B](f: A => Calc[R, I, S2, S3, O, E2, B],
                        h: E => Calc[R, I, S2, S3, O, E2, B]): Calc[R, I, S1, S3, O, E2, B] =
      Cont(calc, f, h)
    def flatMap[B](f: A => Calc[R, I, S2, S2, O, E, B]): Calc[R, I, S1, S2, O, E, B] = cont(f, raise(_: E))
    def handleWith(f: E => Calc[R, I, S2, S2, O, E, A]): Calc[R, I, S1, S2, O, E, A] = cont(pure(_: A), f)
    def handle(f: E => A): Calc[R, I, S1, S2, O, E, A]                               = handleWith(e => pure(f(e)))
    def map[B](f: A => B): Calc[R, I, S1, S2, O, E, B]                               = flatMap(a => pure(f(a)))
  }

  implicit class successfullOps[R, I, S1, S2, O, A](val calc: Calc[R, I, S1, S2, O, Nothing, A]) extends AnyVal {
    def flatMapS[S3, B, E](f: A => Calc[R, I, S2, S3, O, E, B]): Calc[R, I, S1, S3, O, E, B] =
      calc.cont(f, (void: Nothing) => void)
  }

  def run[R, I, S1, S2, O, E, A](calc: Calc[R, I, S1, S2, O, E, A], r: R, init: S1): (S2, Either[E, A]) =
    calc match {
      case res: CalcRes[R, I, S1, S2, O, E, A] =>
        res.submit(r, init, (s2, e) => (s2, Left(e)), (s2, a) => (s2, Right(a)))
      case Defer(f) => run(f(), r, init)
      case c @ Cont(src, ks, ke) =>
        src match {
          case res: CalcRes[R, I, S1, c.MidState, O, c.MidErr, c.MidState] =>
            val (sm, next) =
              res.submit[(c.MidState, Calc[R, I, c.MidState, S2, O, E, A])](
                r,
                init,
                (sm, e) => (sm, ke(e)),
                (sm, a) => (sm, ks(a))
              )
            run(next, r, sm)
          case Defer(f) => run(f().cont(ks, ke), r, init)
          case Cont(src1, ks1, ke1) =>
            run(src1.cont(a => ks1(a).cont(ks, ke), e => ke1(e).cont(ks, ke)), r, init)
        }
    }

  implicit def calcInstance[R, I, S, O, E]: CalcFunctorInstance[R, I, S, O, E] = new CalcFunctorInstance[R, I, S, O, E]

  class CalcFunctorInstance[R, I, S, O, E]
      extends MonadError[Calc[R, I, S, S, O, E, ?], E] with cats.Defer[Calc[R, I, S, S, O, E, ?]]
      with StackSafeMonad[Calc[R, I, S, S, O, E, ?]] {
    def defer[A](fa: => Calc[R, I, S, S, O, E, A]): Calc[R, I, S, S, O, E, A] = Calc.defer(fa)
    def raiseError[A](e: E): Calc[R, I, S, S, O, E, A]                        = Calc.raise(e)
    def handleErrorWith[A](fa: Calc[R, I, S, S, O, E, A])(
        f: E => Calc[R, I, S, S, O, E, A]): Calc[R, I, S, S, O, E, A] = fa.handleWith(f)
    def flatMap[A, B](fa: Calc[R, I, S, S, O, E, A])(f: A => Calc[R, I, S, S, O, E, B]): Calc[R, I, S, S, O, E, B] =
      fa.flatMap(f)
    def pure[A](x: A): Calc[R, I, S, S, O, E, A] = Calc.pure(x)
  }
}
