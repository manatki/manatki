package manatki.data.eval

import cats.data.{IndexedState, State}
import cats.evidence.Is
import cats.{Functor, Monad, MonadError, Monoid, StackSafeMonad}
import manatki.free.FunK
import CalcTSpecials._

import scala.annotation.tailrec

sealed trait CalcT[+F[+_], -R, S, +O, +E, +A] extends SinkStep[F, Any, Any, Any, R, S, O, E, A] {
  def narrowRead[R1 <: R]: CalcT[F, R1, S, O, E, A] = this
  def mapK[G[+_]](fk: FunK[F, G]): CalcT[G, R, S, O, E, A]
}

object CalcT {
  def unit[S]: CalcT[Nothing, Any, S, Nothing, Nothing, Unit]       = Pure(())
  def pure[S, A](a: A): CalcT[Nothing, Any, S, Nothing, Nothing, A] = Pure(a)
  def read[S, R]: CalcT[Nothing, R, S, Nothing, Nothing, R]         = Read()
  def get[S]: CalcT[Nothing, Any, S, Nothing, Nothing, S]           = Get()
  def set[S](s: S): CalcT[Nothing, Any, S, Nothing, Nothing, Unit]  = Set(s)
  def update[S](f: S => S): CalcT[Nothing, Any, S, Nothing, Nothing, Unit] =
    get[S].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): CalcT[Nothing, Any, S, Nothing, E, Nothing]    = Raise(e)
  def defer[F[+_], R, S, O, E, A](x: => CalcT[F, R, S, O, E, A])        = Defer(() => x)
  def delay[S, A](x: => A): CalcT[Nothing, Any, S, Nothing, Nothing, A] = defer(pure[S, A](x))

  def write[S](s: S)(implicit S: Monoid[S]): CalcT[Nothing, Any, S, Nothing, Nothing, Unit] = update(S.combine(_, s))

  sealed trait CalcTRes[-R, S, +O, +E, +A] extends CalcT[Nothing, R, S, O, E, A] {
    def submit[X](submit: Submit[R, S, E, A, X]): X
    def mapK[G[+_]](fk: FunK[Nothing, G]): CalcT[G, R, S, O, E, A] = this
  }
  final case class Pure[S, +A](a: A) extends CalcTRes[Any, S, Nothing, Nothing, A] {
    def submit[X](submit: Submit[Any, S, Nothing, A, X]): X = submit.success(submit.state, a)
  }
  final case class Read[S, R]() extends CalcTRes[R, S, Nothing, Nothing, R] {
    def submit[X](submit: Submit[R, S, Nothing, R, X]): X = submit.success(submit.state, submit.read)
  }
  final case class Get[S]() extends CalcTRes[Any, S, Nothing, Nothing, S] {
    def submit[X](submit: Submit[Any, S, Nothing, S, X]): X = submit.success(submit.state, submit.state)
  }
  final case class Set[S](s: S) extends CalcTRes[Any, S, Nothing, Nothing, Unit] {
    def submit[X](submit: Submit[Any, S, Nothing, Unit, X]): X = submit.success(s, ())
  }
  final case class Raise[S, E](e: E) extends CalcTRes[Any, S, Nothing, E, Nothing] {
    def submit[X](submit: Submit[Any, S, E, Nothing, X]): X = submit.error(submit.state, e)
  }
  final case class Defer[+F[+_], -R, S, +O, +E, +A](runStep: () => CalcT[F, R, S, O, E, A])
      extends CalcT[F, R, S, O, E, A] {
    def mapK[G[+_]](fk: FunK[F, G]): CalcT[G, R, S, O, E, A] = Defer(() => runStep().mapK(fk))
  }
  final case class Output[+F[+_], -R, S, +O, +E, +A](
      output: O,
      next: CalcT[F, R, S, O, E, A]
  ) extends CalcT[F, R, S, O, E, A] {
    def mapK[G[+_]](fk: FunK[F, G]) = Output(output, next.mapK(fk))
  }

  sealed trait ProvideT[+F[+_], R, S, +O, +E, +A] extends CalcT[F, R, S, O, E, A] {
    type R1
    def r: R1
    def inner: CalcT[F, R1, S, O, E, A]
    def any: R Is Any
  }

  final case class Provide[+F[+_], R, S, O, +E, +A](r: R, inner: CalcT[F, R, S, O, E, A])
      extends ProvideT[F, Any, S, O, E, A] {
    type R1 = R
    def any                                                    = Is.refl
    def mapK[G[+_]](fk: FunK[F, G]): CalcT[G, Any, S, O, E, A] = Provide(r, inner.mapK(fk))
  }

  final case class Sub[+F[+_], -R, S, +O, +E, M, +A](fm: F[M], k: M => CalcT[F, R, S, O, E, A])
      extends CalcT[F, R, S, O, E, A] {
    def mapK[G[+_]](fk: FunK[F, G]): CalcT[G, R, S, O, E, A] =
      Sub[G, R, S, O, E, M, A](fk(fm), m => k(m).mapK(fk))
  }

  final case class Bind[+F[+_], -R, S, O1, +O2, E1, +E2, A, +B](
      src: CalcT[F, R, S, O1, E1, A],
      continue: Continue[F, O1, E1, A, R, S, O2, E2, B],
  ) extends CalcT[F, R, S, O2, E2, B] {
    type MidErr = E1
    type MidVal = A
    type MidOut = O1

    def mapK[G[+_]](fk: FunK[F, G]): CalcT[G, R, S, O2, E2, B] =
      Bind(src.mapK(fk), continue.mapK(fk))
  }

  @tailrec private def runTRS[F[+_], R, S, O, E, A](
      acc: List[O],
      s: S,
      res: StepResult[F, S, O, E, A]
  )(implicit F: Monad[F]): F[Either[(List[O], S, CalcT[F, Any, S, O, E, A]), (List[O], S, Either[E, A])]] = res match {
    case now: StepResult.Now[S, E, A]       => F.pure(Right((acc.reverse, now.state, now.result)))
    case y: StepResult.Yield[F, S, O, E, A] => runTRS(y.head :: acc, s, y.tail())
    case wrap: StepResult.Wrap[F, r, S, O, E, m, A] =>
      F.map(wrap.provided)(c => Left((acc, wrap.state, c)))
  }

  implicit class invariantOps[F[+_], R, S, O, E, A](private val calc: CalcT[F, R, S, O, E, A]) extends AnyVal {
    final def step(r: R, init: S)(implicit F: Functor[F]): StepResult[F, S, O, E, A] = CalcT.step(calc, r, init)

    final def runTailRec(rec: R, init: S)(implicit F: Monad[F]): F[(List[O], S, Either[E, A])] =
      F.tailRecM((List[O](), init, calc.provide(rec))) {
        case (acc, s, c) => runTRS(acc, s, c.step(rec, s))
      }

    final def runUnit(init: S)(implicit ev: Unit <:< R, F: Functor[F]): StepResult[F, S, O, E, A] = step((), init)

    def bind[R1 <: R, O2, E2, B](
        continue: Continue[F, O, E, A, R1, S, O2, E2, B]
    ): CalcT[F, R1, S, O2, E2, B] =
      Bind(calc, continue)
    def flatMap[R1 <: R, E1 >: E, O1 >: O, B](
        f: A => CalcT[F, R1, S, O1, E1, B]
    ): CalcT[F, R1, S, O1, E1, B] =
      bind[R1, O1, E1, B](Continue.flatMapConst[F, A, R1, S, O1, E1, B](f))
    def >>=[R1 <: R, E1 >: E, O1 >: O, B](f: A => CalcT[F, R1, S, O1, E1, B]) = flatMap(f)
    def >>[R1 <: R, E1 >: E, O1 >: O, B](c: => CalcT[F, R1, S, O1, E1, B])    = flatMap(_ => c)
    def handleWith[R1 <: R, A1 >: A, O1 >: O, E1](
        f: E => CalcT[F, R1, S, O1, E1, A1]
    ): CalcT[F, R1, S, O1, E1, A1]                            = bind(Continue.handleWithConst[F, E, R1, S, O1, E1, A1](f))
    def handle(f: E => A): CalcT[F, R, S, O, E, A]            = handleWith(e => pure(f(e)))
    def map[B](f: A => B): CalcT[F, R, S, O, E, B]            = flatMap(a => pure(f(a)))
    def as[B](b: => B): CalcT[F, R, S, O, E, B]               = map(_ => b)
    def mapError[E1](f: E => E1): CalcT[F, R, S, O, E1, A]    = handleWith(e => CalcT.raise(f(e)))
    def provide(r: R): CalcT[F, Any, S, O, E, A]              = Provide(r, calc)
    def provideSet(r: R, s: S): CalcT[F, Any, S, O, E, A]     = set(s) *>> calc.provide(r)
    def provideSome[R1](f: R1 => R): CalcT[F, R1, S, O, E, A] = read[S, R1] flatMapS (r => calc.provide(f(r)))

//    def focus[S1](lens: Contains[S, S1]): CalcT[F, R, S1, O, E, A] =
//      get[S1].flatMapS { s1 =>
//        set(lens.extract(s3)) *>> calc.bind(
//          new Continue[F, O, E, A, R, S2, S4, O, E, A] {
//            def success(result: A): CalcT[F, R, S2, S4, O, E, A] =
//              get[S2].flatMapS(s2 => set(lens.set(s3, s2)) *>> pure(result))
//            def error(err: E): CalcT[F, R, S2, S4, O, E, A] =
//              get[S2].flatMapS(s2 => set(lens.set(s3, s2)) *>> raise(err))
//            def output(out: O): CalcT[F, R, S2, S2, O, E, SinkStep[F, O, E, A, R, S2, S4, O, E, A]] = ???
//          }
//        )
//      }
  }

  implicit class CalcSuccessfullOps[F[+_], R, S, O, A](private val calc: CalcT[F, R, S, O, Nothing, A]) extends AnyVal {
    final def flatMapS[R1 <: R, B, O1 >: O, E](
        f: A => CalcT[F, R1, S, O1, E, B]
    ): CalcT[F, R1, S, O1, E, B] =
      calc.bind(Continue.flatMapSuccess[F, A, R1, S, O1, E, B](f))
    final def productRS[R1 <: R, S3, O1 >: O, B, E](
        r: => CalcT[F, R1, S, O1, E, B]
    ): CalcT[F, R1, S, O1, E, B] =
      flatMapS(_ => r)
    final def *>>[R1 <: R, S3, O1 >: O, B, E](r: => CalcT[F, R1, S, O1, E, B]): CalcT[F, R1, S, O1, E, B] =
      productRS(r)
  }

  implicit class CalcPureOps[R, S, O, E, A](private val calc: CalcT[NothingT, R, S, O, E, A]) extends AnyVal {
    final def run(r: R, init: S): (List[O], S, Either[E, A]) = {
      def go(acc: List[O], s: StepResult[NothingT, S, O, E, A]): (List[O], S, Either[E, A]) =
        s match {
          case wrap @ StepResult.Wrap(_, _, n, _) => n
          case StepResult.Error(s, err)           => (acc.reverse, s, Left(err))
          case StepResult.Ok(s, a)                => (acc.reverse, s, Right(a))
          case StepResult.Yield(head, tail)       => go(head :: acc, tail())
        }

      go(Nil, calc.step(r, init))
    }
  }

  implicit class CalcPureSuccessfullOps[R, S, A](private val calc: CalcT[Nothing, R, S, Nothing, Nothing, A])
      extends AnyVal {
    final def runSuccess(r: R, init: S): (S, A) =
      calc.step(r, init) match {
        case StepResult.Wrap(_, _, n, _) => n
        case StepResult.Error(_, err)    => err
        case StepResult.Ok(s, a)         => (s, a)
        case StepResult.Yield(n, _)      => n
      }
  }

  implicit class CalcUnsuccessfullOps[F[+_], R, S, O, E](private val calc: CalcT[F, R, S, O, E, Nothing])
      extends AnyVal {
    def handleWithS[R1 <: R, E1, S3, O1 >: O, A](
        f: E => CalcT[F, R, S, O1, E1, A]
    ): CalcT[F, R1, S, O1, E1, A] =
      calc.bind(Continue.handleWithFail[F, E, R1, S, O1, E1, A](f))
  }

  implicit class CalcFixedStateOps[F[+_], R, S, O, E, A](private val calc: CalcT[F, R, S, O, E, A]) extends AnyVal {
    def when(b: Boolean): CalcT[F, R, S, O, E, Any] = if (b) calc else CalcT.unit
  }

  implicit class CalcSimpleStateOps[F[+_], S, A](
      private val calc: CalcT[Nothing, Any, S, Nothing, Nothing, A]
  ) extends AnyVal {
    final def runSuccessUnit(init: S): (S, A) = calc.runSuccess((), init)

    def toState: State[S, A] = IndexedState(runSuccessUnit)
  }

  def stepRec[F[+_]: Functor, R, S, O, E, A](
      calc: CalcT[F, R, S, O, E, A],
      r: R,
      init: S
  ): StepResult[F, S, O, E, A] = step(calc, r, init)

  @tailrec
  def step[F[+_], R, S, O, E, A](calc: CalcT[F, R, S, O, E, A], r: R, init: S)(
      implicit F: Functor[F]
  ): StepResult[F, S, O, E, A] =
    calc match {
      case res: CalcTRes[R, S, O, E, A] =>
        res.submit(new Submit[R, S, E, A, StepResult[F, S, O, E, A]](r, init) {
          def success(s: S, a: A) = StepResult.Ok(s, a)
          def error(s: S, err: E) = StepResult.Error(s, err)
        })
      case d: Defer[F, R, S, O, E, A]    => step(d.runStep(), r, init)
      case sub: Sub[F, R, S, O, E, m, A] => StepResult.Wrap(r, init, sub.fm, sub.k)
      case p: Provide[F, r, S, O, E, A]  => step[F, r, S, O, E, A](p.inner, p.r, init)
      case o: Output[F, R, S, O, E, A]   => StepResult.Yield(o.output, () => stepRec(o.next, r, init))
      case c1: Bind[F, R, S, o1, O, e1, E, a1, A] =>
        c1.src match {
          case res: CalcTRes[R, S, o1, e1, a1] =>
            val (sm, next) =
              res.submit[(S, CalcT[F, R, S, O, E, A])](
                new Submit[R, S, e1, a1, (S, CalcT[F, R, S, O, E, A])](r, init) {
                  def success(s: S, a: a1) = (s, c1.continue.success(a))
                  def error(s: S, err: e1) = (s, c1.continue.error(err))
                }
              )
            step[F, R, S, O, E, A](next, r, sm)
          case d: Defer[F, R, S, _, _, _] => step(d.runStep().bind(c1.continue), r, init)
          case sub: Sub[F, R, S, _, _, m, _] =>
            StepResult.Wrap[F, R, S, O, E, m, A](r, init, sub.fm, m => sub.k(m).bind(c1.continue))
          case p: ProvideT[F, R, S, _, _, _] =>
            val kcont = p.any.substitute[λ[r => Continue[F, o1, e1, a1, r, S, O, E, A]]](c1.continue)

            step(p.inner.bind[p.R1, O, E, A](kcont), p.r, init)
          case o: Output[F, R, S, c1.MidOut, c1.MidErr, c1.MidVal] =>
            val cont =
              new Continue[F, O, E, SinkStep[F, o1, e1, a1, R, S, O, E, A], R, S, O, E, A] {
                def success(result: SinkStep[F, o1, e1, a1, R, S, O, E, A]): CalcT[F, R, S, O, E, A] = result match {
                  case calc: CalcT[F, R, S, O, E, A]                                         => calc
                  case continue: Continue[F, c1.MidOut, c1.MidErr, c1.MidVal, R, S, O, E, A] => o.next.bind(continue)
                }
                def error(err: E): CalcT[F, R, S, O, E, A] = CalcT.raise(err)
                def output(
                    out: O
                ): CalcT[F, R, S, O, E, SinkStep[F, O, E, SinkStep[F, o1, e1, a1, R, S, O, E, A], R, S, O, E, A]] =
                  Output(out, CalcT.pure(this))
              }
            step(c1.continue.output(o.output).bind(cont), r, init)
          case c2: Bind[F, R, S, o2, c1.MidOut, e2, c1.MidErr, a2, c1.MidVal] =>
            step(c2.src.bind(Continue.compose(c2.continue, c1.continue)), r, init)
        }
    }

  implicit def calcInstance[F[+_], R, S, O, E]: CalcFunctorInstance[F, R, S, O, E] =
    new CalcFunctorInstance[F, R, S, O, E]

  class CalcFunctorInstance[F[+_], R, S, O, E]
      extends MonadError[CalcT[F, R, S, O, E, *], E] with cats.Defer[CalcT[F, R, S, O, E, *]]
      with StackSafeMonad[CalcT[F, R, S, O, E, *]] {
    def defer[A](fa: => CalcT[F, R, S, O, E, A]): CalcT[F, R, S, O, E, A] = CalcT.defer(fa)
    def raiseError[A](e: E): CalcT[F, R, S, O, E, A]                      = CalcT.raise(e)
    def handleErrorWith[A](
        fa: CalcT[F, R, S, O, E, A]
    )(f: E => CalcT[F, R, S, O, E, A]): CalcT[F, R, S, O, E, A] =
      fa.handleWith(f)
    def flatMap[A, B](fa: CalcT[F, R, S, O, E, A])(f: A => CalcT[F, R, S, O, E, B]): CalcT[F, R, S, O, E, B] =
      fa.flatMap(f)
    def pure[A](x: A): CalcT[F, R, S, O, E, A] = CalcT.pure(x)
//    def bracketCase[A, B](
//        acquire: CalcT[F, R, S, O, E, A]
//    )(
//        use: A => CalcT[F, R, S, O, E, B]
//    )(release: (A, ExitCase[E]) => CalcT[F, R, S, O, E, Unit]): CalcT[F, R, S, O, E, B] =
//      acquire.flatMap { a =>
//        use(a).bind(new Continue[F, O, E, B, R, S, O, E, B] {
//          def success(result: B): CalcT[F, R, S, O, E, B]                               = release(a, ExitCase.Completed).as(result)
//          def error(err: E): CalcT[F, R, S, O, E, B]                                    = release(a, ExitCase.Error(err)) >> CalcT.raise(err)
//          def output(out: O): CalcT[F, R, S, O, E, SinkStep[F, O, E, B, R, S, O, E, B]] = Output(out, Pure(this))
//        })
//      }
  }
}

object CalcTSpecials {
  sealed trait SinkStep[+F[+_], -XO, -XE, -XA, -R, S, +O, +E, +A] {
    def mapK[G[+_]](f: FunK[F, G]): SinkStep[G, XO, XE, XA, R, S, O, E, A]
  }

  trait Continue[+F[+_], -XO, -XE, -XA, -R, S, +O, +E, +A] extends SinkStep[F, XO, XE, XA, R, S, O, E, A] {
    self =>
    def success(result: XA): CalcT[F, R, S, O, E, A]
    def error(err: XE): CalcT[F, R, S, O, E, A]
    def output(out: XO): CalcT[F, R, S, O, E, SinkStep[F, XO, XE, XA, R, S, O, E, A]]

    def mapK[G[+_]](f: FunK[F, G]) = new Continue[G, XO, XE, XA, R, S, O, E, A] {
      def success(result: XA): CalcT[G, R, S, O, E, A] = self.success(result).mapK(f)
      def error(err: XE): CalcT[G, R, S, O, E, A]      = self.error(err).mapK(f)
      def output(out: XO): CalcT[G, R, S, O, E, SinkStep[G, XO, XE, XA, R, S, O, E, A]] =
        self.output(out).mapK(f).map(_.mapK(f))
    }
  }

  object Continue {
    def compose[F[+_], A, B, C, E, V, W, O, P, Q, R, S](
        c1: Continue[F, O, E, A, R, S, P, V, B],
        c2: Continue[F, P, V, B, R, S, Q, W, C],
    ): Continue[F, O, E, A, R, S, Q, W, C] = new Continue[F, O, E, A, R, S, Q, W, C] {
      type SS1 = SinkStep[F, O, E, A, R, S, P, V, B]
      type SS2 = SinkStep[F, O, E, A, R, S, Q, W, C]

      def success(result: A): CalcT[F, R, S, Q, W, C] = c1.success(result).bind(c2)
      def error(err: E): CalcT[F, R, S, Q, W, C]      = c1.error(err).bind(c2)
      def output(out: O): CalcT[F, R, S, Q, W, SinkStep[F, O, E, A, R, S, Q, W, C]] =
        c1.output(out)
          .bind(
            new Continue[F, P, V, SS1, R, S, Q, W, SS2] {
              def success(result: SS1): CalcT[F, R, S, Q, W, SS2] = result match {
                case m: CalcT[F, R, S, P, V, B]               => CalcT.pure(m.bind(c2))
                case c1a: Continue[F, O, E, A, R, S, P, V, B] => CalcT.pure(compose(c1a, c2))
              }
              def error(err: V): CalcT[F, R, S, Q, W, SS2] = CalcT.pure(c2.error(err))

              def output(out: P): CalcT[F, R, S, Q, W, SinkStep[F, P, V, SS1, R, S, Q, W, SS2]] =
                c2.output(out).map {
                  case calc: CalcT[F, R, S, Q, W, C]            => CalcT.pure(calc)
                  case c2a: Continue[F, P, V, B, R, S, Q, W, C] => CalcT.pure(compose(c1, c2a))
                }
            }
          )
    }

    def flatMapConst[F[+_], A, R, S, O, E, B](
        f: A => CalcT[F, R, S, O, E, B]
    ): Continue[F, O, E, A, R, S, O, E, B] = new Continue[F, O, E, A, R, S, O, E, B] {
      def success(result: A): CalcT[F, R, S, O, E, B]                               = f(result)
      def error(err: E): CalcT[F, R, S, O, E, B]                                    = CalcT.Raise[S, E](err)
      def output(out: O): CalcT[F, R, S, O, E, SinkStep[F, O, E, A, R, S, O, E, B]] = CalcT.pure(this)
    }

    def handleWithConst[F[+_], E, R, S, O, V, A](
        f: E => CalcT[F, R, S, O, V, A]
    ): Continue[F, O, E, A, R, S, O, V, A] = new Continue[F, O, E, A, R, S, O, V, A] {
      def success(result: A): CalcT[F, R, S, O, V, A]                               = CalcT.Pure[S, A](result)
      def error(err: E): CalcT[F, R, S, O, V, A]                                    = f(err)
      def output(out: O): CalcT[F, R, S, O, V, SinkStep[F, O, E, A, R, S, O, V, A]] = CalcT.pure(this)
    }

    def flatMapSuccess[F[+_], A, R, S, O, E, B](
        f: A => CalcT[F, R, S, O, E, B]
    ): Continue[F, O, Nothing, A, R, S, O, E, B] = new Continue[F, O, Nothing, A, R, S, O, E, B] {
      def success(result: A): CalcT[F, R, S, O, E, B]                                     = f(result)
      def error(err: Nothing): CalcT[F, R, S, O, E, B]                                    = err
      def output(out: O): CalcT[F, R, S, O, E, SinkStep[F, O, Nothing, A, R, S, O, E, B]] = CalcT.pure(this)
    }

    def handleWithFail[F[+_], E, R, S, O, V, A](
        f: E => CalcT[F, R, S, O, V, A]
    ): Continue[F, O, E, Nothing, R, S, O, V, A] = new Continue[F, O, E, Nothing, R, S, O, V, A] {
      def success(result: Nothing): CalcT[F, R, S, O, V, A]                               = result
      def error(err: E): CalcT[F, R, S, O, V, A]                                          = f(err)
      def output(out: O): CalcT[F, R, S, O, V, SinkStep[F, O, E, Nothing, R, S, O, V, A]] = CalcT.pure(this)
    }
  }

  sealed trait StepResult[+F[+_], +S, +O, +E, +A]

  object StepResult {
    sealed trait Now[+S, +E, +A] extends StepResult[Nothing, S, Nothing, E, A] {
      def state: S
      def result: Either[E, A] = this match {
        case Ok(_, a)    => Right(a)
        case Error(_, e) => Left(e)
      }
    }

    final case class Ok[+S, +A](state: S, value: A)    extends Now[S, Nothing, A]
    final case class Error[+S, +E](state: S, error: E) extends Now[S, E, Nothing]
    final case class Yield[F[+_], S, O, E, A](head: O, tail: () => StepResult[F, S, O, E, A])
        extends StepResult[F, S, O, E, A]

    final case class Wrap[F[+_], R, S, +O, +E, M, +A](
        input: R,
        state: S,
        inner: F[M],
        k: M => CalcT[F, R, S, O, E, A]
    ) extends StepResult[F, S, O, E, A] {
      def provided(implicit F: Functor[F]): F[CalcT[F, Any, S, O, E, A]] =
        F.map(inner)(m => k(m).provide(input))
    }
  }

  abstract class Submit[+R, S, -E, -A, +X](val read: R, val state: S) {
    def success(s: S, a: A): X
    def error(s: S, err: E): X
  }
}
