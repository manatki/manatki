package manatki.data.eval

import java.util.concurrent.atomic.AtomicReference

import cats.{StackSafeMonad, effect}
import cats.effect.{Async, CancelToken, Concurrent, ExitCase}
import cats.kernel.Monoid
import cats.syntax.apply._
import cats.syntax.functor._
import manatki.data.eval.MIO.Result

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise, ExecutionContext => EC}
import scala.util.control.NonFatal

sealed trait MIO[-R, -I, +O, C, +E, +A] {
  final def run(r: R, init: I)(implicit ec: EC): Future[Result[O, C, E, A]] =
    MIO.runCancelable(this)(r, init)._1

  final def runUnit(init: I)(implicit ev: Unit <:< R, ec: EC) = run((), init)
}

object MIO {
  def pure[S, C, A](a: A): MIO[Any, S, S, C, Nothing, A]                        = Pure(a)
  def read[R, C, S]: MIO[R, S, S, C, Nothing, R]                                = Read()
  def info[R, C, S]: MIO[R, S, S, C, Nothing, (R, S, EC)]                       = Info()
  def get[C, S]: MIO[Any, S, S, C, Nothing, S]                                  = Get()
  def set[C, S](s: S): MIO[Any, Any, S, C, Nothing, Unit]                       = Set(s)
  def update[I, O, C](f: I => O): MIO[Any, I, O, C, Nothing, Unit]              = get[C, I].flatMapS(s => set(f(s)))
  def raise[S, E, C](e: E): MIO[Any, S, S, C, E, Nothing]                       = Raise(e)
  def defer[R, I, O, C, E, A](x: => MIO[R, I, O, C, E, A])                      = Defer(() => x)
  def delay[S, C, A](x: => A): MIO[Any, S, S, C, Nothing, A]                    = defer(pure(x))
  def exec[S, C]: MIO[Any, S, S, C, Nothing, EC]                                = Exec()
  def exception[C](exc: Throwable): MIO[Any, Any, Nothing, C, Nothing, Nothing] = Exception(exc)
  def fromFuture[S, C, A](fut: Future[A]): MIO[Any, S, S, C, Nothing, A] = fut.value match {
    case Some(util.Success(value)) => pure[S, C, A](value)
    case Some(util.Failure(exc))   => exception(exc)
    case None =>
      info[Any, C, S].flatMap {
        case (_, s, ec) =>
          Await(cb =>
            fut.onComplete {
              case util.Success(a) => cb.completed(s, a)
              case util.Failure(e) => cb.broken(e)
            }(ec))
      }
  }
  def deferFuture[S, C, A](fut: => Future[A]): MIO[Any, S, S, C, Nothing, A] = defer(fromFuture(fut))

  def write[C, S](s: S)(implicit S: Monoid[S]): MIO[Any, S, S, C, Nothing, Unit] = update(S.combine(_, s))

  type Action[C]  = MIO[Any, Any, Any, C, Nothing, Any]
  type Catcher[C] = Either[C, Throwable] => Action[C]

  sealed trait Result[+S, +C, +E, +A]
  final case class Finished[S, A](state: S, value: A) extends Result[S, Nothing, Nothing, A]
  final case class Failed[S, E](state: S, error: E)   extends Result[S, Nothing, E, Nothing]
  final case class Interrupted[C](cancel: C)          extends Result[Nothing, C, Nothing, Nothing]

  sealed trait MIOSimple[-R, -I, +O, C, +E, A] extends MIO[R, I, O, C, E, A] {
    private[MIO] def respond(s: I, r: R, ec: EC, cb: Callback[O, C, E, A]): Unit
    private[MIO] def contf[X](s: I, r: R, ec: EC, f: A => X, h: E => X): X
  }

  final case class Pure[S, C, A](a: A) extends MIOSimple[Any, S, S, C, Nothing, A] {
    private[MIO] def respond(s: S, r: Any, ec: EC, cb: Callback[S, C, Nothing, A]): Unit = cb.completed(s, a)
    private[MIO] def contf[X](s: S, r: Any, ec: EC, f: A => X, h: Nothing => X)          = f(a)
  }
  final case class Read[S, C, R]() extends MIOSimple[R, S, S, C, Nothing, R] {
    private[MIO] def respond(s: S, r: R, ec: EC, cb: Callback[S, C, Nothing, R]): Unit = cb.completed(s, r)
    private[MIO] def contf[X](s: S, r: R, ec: EC, f: R => X, h: Nothing => X)          = f(r)
  }
  final case class Get[C, S]() extends MIOSimple[Any, S, S, C, Nothing, S] {
    private[MIO] def respond(s: S, r: Any, ec: EC, cb: Callback[S, C, Nothing, S]): Unit = cb.completed(s, s)
    private[MIO] def contf[X](s: S, r: Any, ec: EC, f: S => X, h: Nothing => X)          = f(s)
  }

  final case class Info[R, S, C]() extends MIOSimple[R, S, S, C, Nothing, (R, S, EC)] {
    private[MIO] def respond(s: S, r: R, ec: EC, cb: Callback[S, C, Nothing, (R, S, EC)]): Unit =
      cb.completed(s, (r, s, ec))
    private[MIO] def contf[X](s: S, r: R, ec: EC, f: ((R, S, EC)) => X, h: Nothing => X) = f((r, s, ec))
  }
  final case class Set[C, S](s: S) extends MIOSimple[Any, Any, S, C, Nothing, Unit] {
    private[MIO] def respond(old: Any, r: Any, ec: EC, cb: Callback[S, C, Nothing, Unit]): Unit =
      cb.completed(s, ())
    private[MIO] def contf[X](old: Any, r: Any, ec: EC, f: Unit => X, h: Nothing => X) = f(())
  }
  final case class Raise[S, E, C](e: E) extends MIOSimple[Any, S, S, C, E, Nothing] {
    private[MIO] def respond(s: S, r: Any, ec: EC, cb: Callback[S, C, E, Nothing]): Unit = cb.raised(s, e)
    private[MIO] def contf[X](s: S, r: Any, ec: EC, f: Nothing => X, h: E => X)          = h(e)
  }

  sealed trait MIOBreak[R, I, O, C, E, A]                                  extends MIO[R, I, O, C, E, A] with Next[Nothing, C, Nothing, Nothing]
  final case class Exception[S, C](exc: Throwable)                         extends MIOBreak[Any, Any, Nothing, C, Nothing, Nothing]
  final case class Interrupt[C](cancel: C)                                 extends MIOBreak[Any, Any, Nothing, C, Nothing, Nothing]
  final case class Defer[R, I, O, C, E, A](e: () => MIO[R, I, O, C, E, A]) extends MIO[R, I, O, C, E, A]
  final case class Exec[C, S]() extends MIOSimple[Any, S, S, C, Nothing, EC] {
    private[MIO] def respond(s: S, r: Any, ec: EC, cb: Callback[S, C, Nothing, EC]): Unit = cb.completed(s, ec)
    private[MIO] def contf[X](s: S, r: Any, ec: EC, f: EC => X, h: Nothing => X)          = f(ec)
  }
  final case class Cont[R, I, M, O, EM, C, E, A, B](
      src: MIO[R, I, M, C, EM, A],
      ksuc: A => MIO[R, M, O, C, E, B],
      kerr: EM => MIO[R, M, O, C, E, B]
  ) extends MIO[R, I, O, C, E, B] {
    type MidState = M
    type MidErr   = EM
    type MidVal   = A
  }
  final case class Await[R, S, C, E, A](kont: Callback[S, C, E, A] => Unit)           extends MIO[R, Any, S, C, E, A]
  final case class Catch[R, I, O, C, E, A](f: Catcher[C], src: MIO[R, I, O, C, E, A]) extends MIO[R, I, O, C, E, A]
  private[MIO] final case class Uncatch[A, R, I, O, C, E, B](a: A, f: A => MIO[R, I, O, C, E, B])
      extends MIO[R, I, O, C, E, B]

  sealed trait SupplyCont[R, I, O, C, E, A] extends MIO[R, I, O, C, E, A] {
    def cont[S3, E2, B](ss: A => MIO[R, O, S3, C, E2, B], ee: E => MIO[R, O, S3, C, E2, B]): Next[S3, C, E2, B]
  }
  final case class Supply[R, I, O, C, E, A](r: R, s: I, m: MIO[R, I, O, C, E, A])
      extends MIO[Any, Any, O, C, E, A] with Next[O, C, E, A] with SupplyCont[R, I, O, C, E, A] {
    def cont[S, E2, B](ss: A => MIO[R, O, S, C, E2, B], ee: E => MIO[R, O, S, C, E2, B]): Next[S, C, E2, B] =
      Supply(r, s, m.cont(ss, ee))
  }

  implicit class invariantOps[R, I, O, C, E, A](val mio: MIO[R, I, O, C, E, A]) extends AnyVal {
    def cont[R2 <: R, E2, S3, B](f: A => MIO[R2, O, S3, C, E2, B],
                                 h: E => MIO[R2, O, S3, C, E2, B]): MIO[R2, I, S3, C, E2, B] =
      Cont(mio, f, h)
    def flatMap[B, R2 <: R](f: A => MIO[R2, O, O, C, E, B]): MIO[R2, I, O, C, E, B] = cont(f, raise(_: E))
    def handleWith[E2](f: E => MIO[R, O, O, C, E2, A]): MIO[R, I, O, C, E2, A]      = cont(pure(_: A), f)
    def handle(f: E => A): MIO[R, I, O, C, Nothing, A]                              = handleWith(e => pure(f(e)))
    def map[B](f: A => B): MIO[R, I, O, C, E, B]                                    = flatMap(a => pure(f(a)))
    def supply(s: I, r: R): MIO[Any, Any, O, C, E, A]                               = Supply(r, s, mio)

  }

  implicit class successfulOps[R, I, O, C, A](val calc: MIO[R, I, O, C, Nothing, A]) extends AnyVal {
    def flatMapS[S3, E, B](f: A => MIO[R, O, S3, C, E, B]): MIO[R, I, S3, C, E, B] =
      calc.cont(f, (void: Nothing) => void)
  }

  trait Callback[-S, -C, -E, -A] { self =>
    def raised(state: S, error: E): Unit
    def completed(state: S, value: A): Unit
    def broken(exception: Throwable): Unit
    def interrupted(cancel: C): Unit
    def +[S1 <: S, C1 <: C, E1 <: E, A1 <: A](other: Callback[S1, C1, E1, A1]): Callback[S1, C1, E1, A1] =
      if (other == Callback.empty) this
      else if (this == Callback.empty) other
      else
        new Callback[S1, C1, E1, A1] {
          def raised(state: S1, error: E1): Unit = {
            self.raised(state, error)
            other.raised(state, error)
          }

          def completed(state: S1, value: A1): Unit = {
            self.completed(state, value)
            other.completed(state, value)
          }
          def broken(exception: Throwable): Unit = {
            self.broken(exception)
            other.broken(exception)
          }
          def interrupted(cancel: C1): Unit = {
            self.interrupted(cancel)
            other.interrupted(cancel)
          }
        }
  }
  object Callback {
    val empty = new Callback[Any, Any, Any, Any] {
      def raised(state: Any, error: Any): Unit    = ()
      def completed(state: Any, value: Any): Unit = ()
      def broken(e: Throwable): Unit              = ()
      def interrupted(cancel: Any): Unit          = ()
    }
  }

  sealed trait Next[+O, C, +E, +A]
  case class Stop[C]() extends Next[Nothing, C, Nothing, Nothing]
  object Next {
    implicit def unitToStop[S, C, E, A](x: Unit): Next[S, C, E, A] = Stop()
  }

  private def reRun[R, I, O, C, E, A](calc: MIO[R, I, O, C, E, A],
                                      r: R,
                                      init: I,
                                      cb: Callback[O, C, E, A],
                                      cancelation: AtomicReference[C],
                                      catches: List[Catcher[C]] = Nil,
                                      rest: List[Action[C]])(implicit ec: EC): Unit =
    run(calc, r, init, cb, cancelation, catches, rest)

  @tailrec def run[R, I, O, C, E, A](
      calc: MIO[R, I, O, C, E, A],
      r: R,
      init: I,
      cb: Callback[O, C, E, A] = Callback.empty,
      cancel: AtomicReference[C] = new AtomicReference[C](),
      catches: List[Catcher[C]] = Nil,
      rest: List[Action[C]] = Nil,
  )(implicit ec: EC): Unit = {
    var cs = catches
    @tailrec def loop[S](m: MIO[R, S, O, C, E, A], s: S): Next[O, C, E, A] =
      if (cancel.get() != null) cb.interrupted(cancel.get())
      else
        m match {
          case simple: MIOSimple[R, S, O, C, E, A] => simple.respond(s, r, ec, cb)
          case break: MIOBreak[R, S, O, C, E, A]   => break
          case Defer(f)                            => loop(f(), s)
          case Await(f)                            => f(cb)
          case Catch(c, src) =>
            cs ::= c
            loop(src, s)
          case Uncatch(a, k) =>
            cs = cs.drop(1)
            loop(k(a), s)
          case s @ Supply(_, _, _) => s
          case c @ Cont(src, ks, ke) =>
            src match {
              case simple: MIOSimple[R, S, c.MidState, C, c.MidErr, c.MidVal] =>
                loop(simple.contf(s, r, ec, ks, ke), s)
              case break: MIOBreak[R, S, c.MidState, C, c.MidErr, c.MidVal] => break
              case sup: SupplyCont[R, S, c.MidState, C, c.MidErr, c.MidVal] =>
                sup.cont(ks, ke)
              case Catch(c1, src1) =>
                cs ::= c1
                loop(src1.cont(a => Uncatch(a, ks), e => Uncatch(e, ke)), s)
              case Uncatch(a, k) =>
                cs = cs.drop(1)
                loop(k(a).cont(ks, ke), s)
              case Defer(f) => loop(f().cont(ks, ke), s)
              case Cont(src1, kI, ke1) =>
                loop(src1.cont(a => kI(a).cont(ks, ke), e => ke1(e).cont(ks, ke)), s)
              case Await(f) =>
                f(new Callback[c.MidState, C, c.MidErr, c.MidVal] {
                  def raised(state: c.MidState, error: c.MidErr): Unit =
                    ec.execute(() => reRun(ke(error), r, state, cb, cancel, cs, rest))
                  def completed(state: c.MidState, value: c.MidVal): Unit =
                    ec.execute(() => reRun(ks(value), r, state, cb, cancel, cs, rest))
                  def broken(e: Throwable): Unit = {
                    for (c <- cs) c(Right(e))
                    cb.broken(e)
                  }
                  def interrupted(cancel: C): Unit = cb.interrupted(cancel)
                })
            }
        }
    val (next, follows): (Next[O, C, E, A], List[Action[C]]) = try {
      loop(calc, init) -> rest
    } catch {
      case NonFatal(e) =>
        cb.broken(e)
        Stop() -> (cs.map(_(Right(e))) ::: rest)
    }

    next match {
      case Stop() =>
        follows match {
          case head :: rest1 => run(head, (), (), rest = rest1)
          case _             =>
        }
      case Exception(ex) =>
        cb.broken(ex)
        cs.map(_(Right(ex))) ::: rest match {
          case head :: rest1 => run(head, (), (), rest = rest1)
          case _             =>
        }
      case Interrupt(c) =>
        cb.interrupted(c)
        cs.map(_(Left(c))) ::: rest match {
          case head :: rest1 => run(head, (), (), rest = rest1)
          case _             =>
        }
      case Supply(r1, s1, m) => run(m, r1, s1, cb, cancel, cs, rest)
    }
  }

  final def runCancelable[R, I, O, C, E, A](mio: MIO[R, I, O, C, E, A], add: Callback[O, C, E, A] = Callback.empty)(
      r: R,
      init: I)(implicit ec: EC): (Future[Result[O, C, E, A]], C => Unit) = {
    val p           = Promise[Result[O, C, E, A]]
    val cancelation = new AtomicReference[C]
    val cb = add + new MIO.Callback[O, C, E, A] {
      def raised(state: O, error: E): Unit    = p.success(Failed(state, error))
      def completed(state: O, value: A): Unit = p.success(Finished(state, value))
      def broken(e: Throwable): Unit          = p.failure(e)
      def interrupted(cancel: C): Unit        = p.success(Interrupted(cancel))
    }
    MIO.run[R, I, O, C, E, A](mio, r, init, cb, cancelation)
    (p.future, cancelation.set)
  }

  final def runConc[R, I, O, C, E, A](mio: MIO[R, I, O, C, E, A]): MIO[R, I, I, C, Nothing, Fiber[O, C, E, A]] =
    info[R, C, I].map {
      case (r, i, ec) =>
        val (fut, setter) = runCancelable(mio)(r, i)(ec)

        new Fiber[O, C, E, A] {
          val join: MIO[Any, Any, O, C, E, A] =
            fromFuture(fut).flatMapS {
              case Finished(s, a) => set(s).flatMap(_ => pure(a))
              case Failed(s, e)   => set(s).flatMap(_ => raise(e))
              case Interrupted(c) => Interrupt(c)
            }
          def cancel[S, C1](cancel: C): MIO[Any, S, S, C1, Nothing, Any] = delay(setter(cancel))
        }
    }

  implicit def mioAsyncInstance[R, S, C, E]: MIOAsyncInstance[R, S, C, E] = new MIOAsyncInstance[R, S, C, E]
  implicit def mioConcurrentInstance[R, S, E]: MIOConcurrentInstance[R, S, E] = new MIOConcurrentInstance[R, S, E]

  class MIOAsyncInstance[R, S, C, E]
      extends cats.Defer[MIO[R, S, S, C, E, ?]] with StackSafeMonad[MIO[R, S, S, C, E, ?]]
      with Async[MIO[R, S, S, C, E, ?]] {
    def suspend[A](fa: => MIO[R, S, S, C, E, A]): MIO[R, S, S, C, E, A]                                = MIO.defer(fa)
    def flatMap[A, B](fa: MIO[R, S, S, C, E, A])(f: A => MIO[R, S, S, C, E, B]): MIO[R, S, S, C, E, B] = fa.flatMap(f)
    def pure[A](x: A): MIO[R, S, S, C, E, A]                                                           = MIO.pure(x)

    def bracketCase[A, B](acquire: MIO[R, S, S, C, E, A])(use: A => MIO[R, S, S, C, E, B])(
        release: (A, ExitCase[Throwable]) => MIO[R, S, S, C, E, Unit]): MIO[R, S, S, C, E, B] =
      (acquire, info[R, C, S]).tupled.flatMap {
        case (a, (r, s, ec)) =>
          Catch(
            (reason: Either[C, Throwable]) => {
              val exit = reason.fold(_ => ExitCase.Canceled, ExitCase.Error(_))
              release(a, exit).supply(s, r).handle(_ => ())
            },
            use(a).cont(
              b => release(a, ExitCase.Completed) as b,
              e => release(a, ExitCase.Error(MIOExcept(e))) *> raise(e)
            )
          )
      }

    def raiseError[A](e: Throwable): MIO[R, S, S, C, E, A] = delay(throw e)

    def handleErrorWith[A](fa: MIO[R, S, S, C, E, A])(f: Throwable => MIO[R, S, S, C, E, A]): MIO[R, S, S, C, E, A] =
      fa.handleWith(e => f(MIOExcept(e)))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): MIO[R, S, S, C, E, A] =
      get[C, S] flatMap (s =>
        Await[R, S, C, E, A](cb =>
          k {
            case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
            case Left(exc)                        => cb.broken(exc)
            case Right(value)                     => cb.completed(s, value)
        }))

    def asyncF[A](k: (Either[Throwable, A] => Unit) => MIO[R, S, S, C, E, Unit]): MIO[R, S, S, C, E, A] =
      info[R, C, S].flatMap {
        case (r, s, ec) =>
          Await[R, S, C, E, A] { cb =>
            val mio = k {
              case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
              case Left(exc)                        => cb.broken(exc)
              case Right(value)                     => cb.completed(s, value)
            }
            run(mio, r, s, Callback.empty)(ec)
          }
      }
  }

  class MIOConcurrentInstance[R, S, E] extends MIOAsyncInstance[R, S, Any, E] with Concurrent[MIO[R, S, S, Any, E, ?]] {
    type F[A] = MIO[R, S, S, Any, E, A]
    private def fiberToEffect[A](fib: Fiber[S, Any, E, A]): effect.Fiber[F, A] = new effect.Fiber[F, A] {
      def cancel: F[Unit] = fib.cancel[S, Any](()).void
      def join: F[A]      = fib.join
    }

    def start[A](fa: F[A]): F[effect.Fiber[F, A]] = runConc(fa).map(fiberToEffect)

    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(A, effect.Fiber[F, B]), (effect.Fiber[F, A], B)]] =
      (runConc(fa), runConc(fb)).tupled.flatMap {
        case (fiba, fibb) =>
          exec[S, Any].flatMap { implicit ec =>
            Await { cb =>
              val cba = new Callback[S, Any, E, A] {
                def raised(state: S, error: E): Unit    = cb.raised(state, error)
                def completed(state: S, value: A): Unit = cb.completed(state, Left(value -> fiberToEffect(fibb)))
                def broken(exception: Throwable): Unit  = cb.broken(exception)
                def interrupted(cancel: Any): Unit      = cb.interrupted(())
              }

              val cbb = new Callback[S, Any, E, B] {
                def raised(state: S, error: E): Unit    = cb.raised(state, error)
                def completed(state: S, value: B): Unit = cb.completed(state, Right(fiberToEffect(fiba) -> value))
                def broken(exception: Throwable): Unit  = cb.broken(exception)
                def interrupted(cancel: Any): Unit      = cb.interrupted(())
              }

              run(fiba.join, (), (), cba)
              run(fibb.join, (), (), cbb)
            }
          }
      }
  }

  final case class MIOExcept[E](e: E) extends Throwable

  trait Fiber[S, C, E, A] { self =>
    def join: MIO[Any, Any, S, C, E, A]
    def cancel[S1, C1](cancel: C): MIO[Any, S1, S1, C1, Nothing, Any]
  }
}
