package manatki.data.eval

import cats.StackSafeMonad
import cats.effect.{Async, ExitCase}
import cats.kernel.Monoid
import cats.syntax.apply._
import cats.syntax.functor._

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise, ExecutionContext => EC}
import scala.util.control.NonFatal

sealed trait MIO_4[-R, -S1, +S2, +E, +A] {
  final def run(r: R, init: S1)(implicit ec: EC): Future[(S2, Either[E, A])] = {
    val p = Promise[(S2, Either[E, A])]()
    val cb = new MIO_4.Callback[S2, E, A] {
      def raised(state: S2, error: E): Unit    = p.success(state -> Left(error))
      def completed(state: S2, value: A): Unit = p.success(state -> Right(value))
      def broken(e: Throwable): Unit           = p.failure(e)
    }
    MIO_4.run[R, S1, S2, E, A](this, r, init, cb)
    p.future
  }
  final def runUnit(init: S1)(implicit ev: Unit <:< R, ec: EC) = run((), init)
}

object MIO_4 {
  def pure[S, A](a: A): MIO_4[Any, S, S, Nothing, A]                 = Pure(a)
  def read[R, S]: MIO_4[R, S, S, Nothing, R]                         = Read()
  def info[R, S]: MIO_4[R, S, S, Nothing, (R, S, EC)]                = Info()
  def get[S]: MIO_4[Any, S, S, Nothing, S]                           = Get()
  def set[S](s: S): MIO_4[Any, Any, S, Nothing, Unit]                = Set(s)
  def update[S1, S2](f: S1 => S2): MIO_4[Any, S1, S2, Nothing, Unit] = get[S1].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): MIO_4[Any, S, S, E, Nothing]                = Raise(e)
  def defer[R, S1, S2, E, A](x: => MIO_4[R, S1, S2, E, A])           = Defer(() => x)
  def delay[S, A](x: => A): MIO_4[Any, S, S, Nothing, A]             = defer(pure(x))
  def exec[S]: MIO_4[Any, S, S, Nothing, EC]                         = Exec()

  def write[S](s: S)(implicit S: Monoid[S]): MIO_4[Any, S, S, Nothing, Unit] = update(S.combine(_, s))

  type Catcher = MIO_4[Any, Any, Any, Nothing, Unit]

  sealed trait MIOSimple[-R, -S1, +S2, +E, A] extends MIO_4[R, S1, S2, E, A] {
    private[MIO_4] def respond(s: S1, r: R, ec: EC, cb: Callback[S2, E, A]): Unit
    private[MIO_4] def contf[X](s: S1, r: R, ec: EC, f: A => X, h: E => X): X
  }

  final case class Pure[S, A](a: A) extends MIOSimple[Any, S, S, Nothing, A] {
    private[MIO_4] def respond(s: S, r: Any, ec: EC, cb: Callback[S, Nothing, A]): Unit = cb.completed(s, a)
    private[MIO_4] def contf[X](s: S, r: Any, ec: EC, f: A => X, h: Nothing => X)       = f(a)
  }
  final case class Read[S, R]() extends MIOSimple[R, S, S, Nothing, R] {
    private[MIO_4] def respond(s: S, r: R, ec: EC, cb: Callback[S, Nothing, R]): Unit = cb.completed(s, r)
    private[MIO_4] def contf[X](s: S, r: R, ec: EC, f: R => X, h: Nothing => X)       = f(r)
  }
  final case class Get[S]() extends MIOSimple[Any, S, S, Nothing, S] {
    private[MIO_4] def respond(s: S, r: Any, ec: EC, cb: Callback[S, Nothing, S]): Unit = cb.completed(s, s)
    private[MIO_4] def contf[X](s: S, r: Any, ec: EC, f: S => X, h: Nothing => X)       = f(s)
  }

  final case class Info[R, S]() extends MIOSimple[R, S, S, Nothing, (R, S, EC)] {
    private[MIO_4] def respond(s: S, r: R, ec: EC, cb: Callback[S, Nothing, (R, S, EC)]): Unit =
      cb.completed(s, (r, s, ec))
    private[MIO_4] def contf[X](s: S, r: R, ec: EC, f: ((R, S, EC)) => X, h: Nothing => X) = f((r, s, ec))
  }
  final case class Set[S](s: S) extends MIOSimple[Any, Any, S, Nothing, Unit] {
    private[MIO_4] def respond(old: Any, r: Any, ec: EC, cb: Callback[S, Nothing, Unit]): Unit = cb.completed(s, ())
    private[MIO_4] def contf[X](old: Any, r: Any, ec: EC, f: Unit => X, h: Nothing => X)       = f(())
  }
  final case class Raise[S, E](e: E) extends MIOSimple[Any, S, S, E, Nothing] {
    private[MIO_4] def respond(s: S, r: Any, ec: EC, cb: Callback[S, E, Nothing]): Unit = cb.raised(s, e)
    private[MIO_4] def contf[X](s: S, r: Any, ec: EC, f: Nothing => X, h: E => X)       = h(e)
  }
  final case class Defer[R, S1, S2, E, A](e: () => MIO_4[R, S1, S2, E, A]) extends MIO_4[R, S1, S2, E, A]
  final case class Exec[S]() extends MIOSimple[Any, S, S, Nothing, EC] {
    private[MIO_4] def respond(s: S, r: Any, ec: EC, cb: Callback[S, Nothing, EC]): Unit = cb.completed(s, ec)
    private[MIO_4] def contf[X](s: S, r: Any, ec: EC, f: EC => X, h: Nothing => X)       = f(ec)
  }
  final case class Cont[R, S1, S2, S3, E1, E2, A, B](
                                                      src: MIO_4[R, S1, S2, E1, A],
                                                      ksuc: A => MIO_4[R, S2, S3, E2, B],
                                                      kerr: E1 => MIO_4[R, S2, S3, E2, B]
  ) extends MIO_4[R, S1, S3, E2, B] {
    type MidState = S2
    type MidErr   = E1
    type MidVal   = A
  }
  final case class Await[R, S, E, A](kont: Callback[S, E, A] => Unit) extends MIO_4[R, Any, S, E, A]
  final case class Catch[R, S1, S2, E, A](f: Throwable => Catcher, src: MIO_4[R, S1, S2, E, A])
      extends MIO_4[R, S1, S2, E, A]
  private[MIO_4] final case class Uncatch[A, R, S1, S2, E, B](a: A, f: A => MIO_4[R, S1, S2, E, B])
      extends MIO_4[R, S1, S2, E, B]


  sealed trait SupplyCont[R, S1, S2, E, A] extends MIO_4[R, S1, S2, E, A] {
    def cont[S3, E2, B](ss: A => MIO_4[R, S2, S3, E2, B], ee: E => MIO_4[R, S2, S3, E2, B]): Next[S3, E2, B]
  }
  final case class Supply[R, S1, S2, E, A](r: R, s: S1, m: MIO_4[R, S1, S2, E, A])
      extends MIO_4[Any, Any, S2, E, A] with Next[S2, E, A] with SupplyCont[R, S1, S2, E, A]{
    def cont[S3, E2, B](ss: A => MIO_4[R, S2, S3, E2, B], ee: E => MIO_4[R, S2, S3, E2, B]): Next[S3, E2, B] =
      Supply(r, s, m.cont(ss, ee))
  }

  implicit class invariantOps[R, S1, S2, E, A](val calc: MIO_4[R, S1, S2, E, A]) extends AnyVal {
    def cont[R2 <: R, E2, S3, B](f: A => MIO_4[R2, S2, S3, E2, B],
                                 h: E => MIO_4[R2, S2, S3, E2, B]): MIO_4[R2, S1, S3, E2, B] =
      Cont(calc, f, h)
    def flatMap[B, R2 <: R](f: A => MIO_4[R2, S2, S2, E, B]): MIO_4[R2, S1, S2, E, B] = cont(f, raise(_: E))
    def handleWith[E2](f: E => MIO_4[R, S2, S2, E2, A]): MIO_4[R, S1, S2, E2, A]      = cont(pure(_: A), f)
    def handle(f: E => A): MIO_4[R, S1, S2, Nothing, A]                             = handleWith(e => pure(f(e)))
    def map[B](f: A => B): MIO_4[R, S1, S2, E, B]                                   = flatMap(a => pure(f(a)))
    def supply(s: S1, r: R): MIO_4[Any, Any, S2, E, A]                              = Supply(r, s, calc)
  }

  implicit class successfulOps[R, S1, S2, A](val calc: MIO_4[R, S1, S2, Nothing, A]) extends AnyVal {
    def flatMapS[S3, E, B](f: A => MIO_4[R, S2, S3, E, B]): MIO_4[R, S1, S3, E, B] =
      calc.cont(f, (void: Nothing) => void)
  }

  trait Callback[-S, -E, -A] {
    def raised(state: S, error: E): Unit
    def completed(state: S, value: A): Unit
    def broken(e: Throwable): Unit
  }
  object Callback {
    val empty = new Callback[Any, Any, Any] {
      def raised(state: Any, error: Any): Unit    = ()
      def completed(state: Any, value: Any): Unit = ()
      def broken(e: Throwable): Unit              = ()
    }
  }

  sealed trait Next[+S2, +E, +A]
  case object Stop extends Next[Nothing, Nothing, Nothing]
  object Next {
    implicit def unitToStop[S, E, A](x: Unit): Next[S, E, A] = Stop
  }

  private def reRun[R, S1, S2, E, A](calc: MIO_4[R, S1, S2, E, A],
                                     r: R,
                                     init: S1,
                                     cb: Callback[S2, E, A],
                                     catches: List[Throwable => Catcher] = Nil,
                                     rest: List[Catcher])(implicit ec: EC): Unit =
    run(calc, r, init, cb, catches, rest)

  @tailrec def run[R, S1, S2, E, A](calc: MIO_4[R, S1, S2, E, A],
                                    r: R,
                                    init: S1,
                                    cb: Callback[S2, E, A],
                                    catches: List[Throwable => Catcher] = Nil,
                                    rest: List[Catcher] = Nil)(implicit ec: EC): Unit = {
    var cs = catches
    @tailrec def loop[S](m: MIO_4[R, S, S2, E, A], s: S): Next[S2, E, A] = m match {
      case simple: MIOSimple[R, S, S2, E, A] => simple.respond(s, r, ec, cb)
      case Defer(f)                          => loop(f(), s)
      case Await(f)                          => f(cb)
      case Catch(c, src) =>
        cs ::= c
        loop(src, s)
      case Uncatch(a, k) =>
        cs = cs.drop(1)
        loop(k(a), s)
      case s @ Supply(_, _, _) => s
      case c @ Cont(src, ks, ke) =>
        src match {
          case simple: MIOSimple[R, S, c.MidState, c.MidErr, c.MidErr] =>
            loop(simple.contf(s, r, ec, ks, ke), s)
          case sup : SupplyCont[R, S, c.MidState, c.MidErr, c.MidVal] =>
            sup.cont(ks, ke)
          case Catch(c1, src1) =>
            cs ::= c1
            loop(src1.cont(a => Uncatch(a, ks), e => Uncatch(e, ke)), s)
          case Uncatch(a, k) =>
            cs = cs.drop(1)
            loop(k(a).cont(ks, ke), s)
          case Defer(f) => loop(f().cont(ks, ke), s)
          case Cont(src1, ks1, ke1) =>
            loop(src1.cont(a => ks1(a).cont(ks, ke), e => ke1(e).cont(ks, ke)), s)
          case Await(f) =>
            f(new Callback[c.MidState, c.MidErr, c.MidVal] {
              def raised(state: c.MidState, error: c.MidErr): Unit =
                ec.execute(() => reRun(ke(error), r, state, cb, cs, rest))
              def completed(state: c.MidState, value: c.MidVal): Unit =
                ec.execute(() => reRun(ks(value), r, state, cb, cs, rest))
              def broken(e: Throwable): Unit = {
                for (c <- cs) c(e)
                cb.broken(e)
              }
            })
        }
    }
    val (next, follows): (Next[S2, E, A], List[Catcher]) = try {
      loop(calc, init) -> rest
    } catch {
      case NonFatal(e) =>
        cb.broken(e)
        Stop -> (cs.map(_(e)) ::: rest)
    }

    next match {
      case Stop =>
        follows match {
          case head :: rest1 => run(head, (), (), Callback.empty, Nil, rest1)
          case _             =>
        }
      case Supply(r1, s1, m) => run(m, r1, s1, cb, cs, rest)
    }
  }

  implicit def calcInstance[R, S, E]: MIOAsyncInstance[R, S, E] = new MIOAsyncInstance[R, S, E]

  class MIOAsyncInstance[R, S, E]
      extends cats.Defer[MIO_4[R, S, S, E, *]] with StackSafeMonad[MIO_4[R, S, S, E, *]] with Async[MIO_4[R, S, S, E, *]] {
    def suspend[A](fa: => MIO_4[R, S, S, E, A]): MIO_4[R, S, S, E, A]                             = MIO_4.defer(fa)
    def flatMap[A, B](fa: MIO_4[R, S, S, E, A])(f: A => MIO_4[R, S, S, E, B]): MIO_4[R, S, S, E, B] = fa.flatMap(f)
    def pure[A](x: A): MIO_4[R, S, S, E, A]                                                     = MIO_4.pure(x)

    def bracketCase[A, B](acquire: MIO_4[R, S, S, E, A])(use: A => MIO_4[R, S, S, E, B])(
        release: (A, ExitCase[Throwable]) => MIO_4[R, S, S, E, Unit]): MIO_4[R, S, S, E, B] =
      (acquire, info[R, S]).tupled.flatMap {
        case (a, (r, s, ec)) =>
          Catch(
            e => release(a, ExitCase.Error(e)).supply(s, r).handle(_ => ()),
            use(a).cont(
              b => release(a, ExitCase.Completed) as b,
              e => release(a, ExitCase.Error(MIOExcept(e))) *> raise(e)
            )
          )
      }

    def raiseError[A](e: Throwable): MIO_4[R, S, S, E, A] = delay(throw e)

    def handleErrorWith[A](fa: MIO_4[R, S, S, E, A])(f: Throwable => MIO_4[R, S, S, E, A]): MIO_4[R, S, S, E, A] =
      fa.handleWith(e => f(MIOExcept(e)))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): MIO_4[R, S, S, E, A] =
      get[S] flatMap (s =>
        Await[R, S, E, A](cb =>
          k {
            case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
            case Left(exc)             => cb.broken(exc)
            case Right(value)          => cb.completed(s, value)
        }))

    def asyncF[A](k: (Either[Throwable, A] => Unit) => MIO_4[R, S, S, E, Unit]): MIO_4[R, S, S, E, A] =
      info[R, S].flatMap {
        case (r, s, ec) =>
          Await[R, S, E, A] { cb =>
            val mio = k {
              case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
              case Left(exc)             => cb.broken(exc)
              case Right(value)          => cb.completed(s, value)
            }
            run(mio, r, s, Callback.empty)(ec)
          }
      }
  }

  final case class MIOExcept[E](e: E) extends Throwable
}
