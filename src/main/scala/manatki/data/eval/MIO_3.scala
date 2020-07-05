package manatki.data.eval

import cats.StackSafeMonad
import cats.effect.{Async, ExitCase}
import cats.evidence.As
import cats.kernel.Monoid
import cats.syntax.apply._
import cats.syntax.functor._

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise, ExecutionContext => EC}
import scala.util.control.NonFatal

sealed trait MIO_3[-R, -S1, +S2, +E, +A] {
  final def run(r: R, init: S1)(implicit ec: EC): Future[(S2, Either[E, A])] = {
    val p = Promise[(S2, Either[E, A])]()
    val cb = new MIO_3.Callback[S2, E, A] {
      def raised(state: S2, error: E): Unit    = p.success(state -> Left(error))
      def completed(state: S2, value: A): Unit = p.success(state -> Right(value))
      def broken(e: Throwable): Unit           = p.failure(e)
    }
    MIO_3.run[R, S1, S2, E, A](this, r, init, cb)
    p.future
  }
  final def runUnit(init: S1)(implicit ev: Unit <:< R, ec: EC) = run((), init)
}

object MIO_3 {
  def pure[S, A](a: A): MIO_3[Any, S, S, Nothing, A]                 = Pure(a)
  def read[R, S]: MIO_3[R, S, S, Nothing, R]                         = Read()
  def info[R, S]: MIO_3[R, S, S, Nothing, (R, S, EC)]                = Info()
  def get[S]: MIO_3[Any, S, S, Nothing, S]                           = Get()
  def set[S](s: S): MIO_3[Any, Any, S, Nothing, Unit]                = Set(s)
  def update[S1, S2](f: S1 => S2): MIO_3[Any, S1, S2, Nothing, Unit] = get[S1].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): MIO_3[Any, S, S, E, Nothing]                = Raise(e)
  def defer[R, S1, S2, E, A](x: => MIO_3[R, S1, S2, E, A])           = Defer(() => x)
  def delay[S, A](x: => A): MIO_3[Any, S, S, Nothing, A]             = defer(pure(x))
  def exec[S]: MIO_3[Any, S, S, Nothing, EC]                         = Exec()

  def write[S](s: S)(implicit S: Monoid[S]): MIO_3[Any, S, S, Nothing, Unit] = update(S.combine(_, s))

  sealed trait MIO3Simple[-R, -S1, +S2, +E, A] extends MIO_3[R, S1, S2, E, A] {
    private[MIO_3] def respond(s: S1, r: R, ec: EC, cb: Callback[S2, E, A]): Unit
    private[MIO_3] def contf[X](s: S1, r: R, ec: EC, f: A => X, h: E => X): X
  }

  final case class Pure[S, A](a: A) extends MIO3Simple[Any, S, S, Nothing, A] {
    private[MIO_3] def respond(s: S, r: Any, ec: EC, cb: Callback[S, Nothing, A]): Unit = cb.completed(s, a)
    private[MIO_3] def contf[X](s: S, r: Any, ec: EC, f: A => X, h: Nothing => X)       = f(a)
  }
  final case class Read[S, R]() extends MIO3Simple[R, S, S, Nothing, R] {
    private[MIO_3] def respond(s: S, r: R, ec: EC, cb: Callback[S, Nothing, R]): Unit = cb.completed(s, r)
    private[MIO_3] def contf[X](s: S, r: R, ec: EC, f: R => X, h: Nothing => X)       = f(r)
  }
  final case class Get[S]() extends MIO3Simple[Any, S, S, Nothing, S] {
    private[MIO_3] def respond(s: S, r: Any, ec: EC, cb: Callback[S, Nothing, S]): Unit = cb.completed(s, s)
    private[MIO_3] def contf[X](s: S, r: Any, ec: EC, f: S => X, h: Nothing => X)       = f(s)
  }

  final case class Info[R, S]() extends MIO3Simple[R, S, S, Nothing, (R, S, EC)] {
    private[MIO_3] def respond(s: S, r: R, ec: EC, cb: Callback[S, Nothing, (R, S, EC)]): Unit =
      cb.completed(s, (r, s, ec))
    private[MIO_3] def contf[X](s: S, r: R, ec: EC, f: ((R, S, EC)) => X, h: Nothing => X) = f((r, s, ec))
  }
  final case class Set[S](s: S) extends MIO3Simple[Any, Any, S, Nothing, Unit] {
    private[MIO_3] def respond(old: Any, r: Any, ec: EC, cb: Callback[S, Nothing, Unit]): Unit = cb.completed(s, ())
    private[MIO_3] def contf[X](old: Any, r: Any, ec: EC, f: Unit => X, h: Nothing => X)       = f(())
  }
  final case class Raise[S, E](e: E) extends MIO3Simple[Any, S, S, E, Nothing] {
    private[MIO_3] def respond(s: S, r: Any, ec: EC, cb: Callback[S, E, Nothing]): Unit = cb.raised(s, e)
    private[MIO_3] def contf[X](s: S, r: Any, ec: EC, f: Nothing => X, h: E => X)       = h(e)
  }
  final case class Defer[R, S1, S2, E, A](e: () => MIO_3[R, S1, S2, E, A]) extends MIO_3[R, S1, S2, E, A]
  final case class Exec[S]() extends MIO3Simple[Any, S, S, Nothing, EC] {
    private[MIO_3] def respond(s: S, r: Any, ec: EC, cb: Callback[S, Nothing, EC]): Unit = cb.completed(s, ec)
    private[MIO_3] def contf[X](s: S, r: Any, ec: EC, f: EC => X, h: Nothing => X)       = f(ec)
  }
  final case class Cont[R, S1, S2, S3, E1, E2, A, B](
      src: MIO_3[R, S1, S2, E1, A],
      ksuc: A => MIO_3[R, S2, S3, E2, B],
      kerr: E1 => MIO_3[R, S2, S3, E2, B]
  ) extends MIO_3[R, S1, S3, E2, B] {
    type MidState = S2
    type MidErr   = E1
    type MidVal   = A
  }
  final case class Await[R, S, E, A](kont: Callback[S, E, A] => Unit)                      extends MIO_3[R, Any, S, E, A]
  final case class Catch[R, S1, S2, E, A](f: Throwable => Unit, src: MIO_3[R, S1, S2, E, A]) extends MIO_3[R, S1, S2, E, A]
  private[MIO_3] final case class Uncatch[A, R, S1, S2, E, B](a: A, f: A => MIO_3[R, S1, S2, E, B])
      extends MIO_3[R, S1, S2, E, B]

  implicit class invariantOps[R, S1, S2, E, A](val calc: MIO_3[R, S1, S2, E, A]) extends AnyVal {
    def cont[R2 <: R, E2, S3, B](f: A => MIO_3[R2, S2, S3, E2, B],
                                 h: E => MIO_3[R2, S2, S3, E2, B]): MIO_3[R2, S1, S3, E2, B] =
      Cont(calc, f, h)
    def flatMap[B, R2 <: R](f: A => MIO_3[R2, S2, S2, E, B]): MIO_3[R2, S1, S2, E, B] = cont(f, raise(_: E))
    def handleWith[E2](f: E => MIO_3[R, S2, S2, E2, A]): MIO_3[R, S1, S2, E2, A]      = cont(pure(_: A), f)
    def handle(f: E => A): MIO_3[R, S1, S2, E, A]                                   = handleWith(e => pure(f(e)))
    def map[B](f: A => B): MIO_3[R, S1, S2, E, B]                                   = flatMap(a => pure(f(a)))
  }

  implicit class successfulOps[R, S1, S2, A](val calc: MIO_3[R, S1, S2, Nothing, A]) extends AnyVal {
    def flatMapS[S3, E, B](f: A => MIO_3[R, S2, S3, E, B]): MIO_3[R, S1, S3, E, B] =
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

  def run[R, S1, S2, E, A](calc: MIO_3[R, S1, S2, E, A],
                           r: R,
                           init: S1,
                           cb: Callback[S2, E, A],
                           catches: List[Throwable => Unit] = Nil)(implicit ec: EC): Unit = {
    var cs = catches
    @tailrec def loop[S](m: MIO_3[R, S, S2, E, A], s: S): Unit = m match {
      case simple: MIO3Simple[R, S, S2, E, A] => simple.respond(s, r, ec, cb)
      case Defer(f)                          => loop(f(), s)
      case Await(f)                          => f(cb)
      case Catch(c, src) =>
        cs ::= c
        loop(src, s)
      case Uncatch(a, k) =>
        cs = cs.drop(1)
        loop(k(a), s)
      case c @ Cont(src, ks, ke) =>
        src match {
          case simple: MIO3Simple[R, S, c.MidState, c.MidErr, c.MidErr] =>
            loop(simple.contf(s, r, ec, ks, ke), s)
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
              def raised(state: c.MidState, error: c.MidErr): Unit = ec.execute(() => run(ke(error), r, state, cb, cs))
              def completed(state: c.MidState, value: c.MidVal): Unit =
                ec.execute(() => run(ks(value), r, state, cb, cs))
              def broken(e: Throwable): Unit = {
                for (c <- cs) c(e)
                cb.broken(e)
              }
            })
        }
    }
    try {
      loop(calc, init)
    } catch {
      case NonFatal(e) =>
        for (c <- cs) c(e)
        cb.broken(e)
    }
  }

  implicit def calcInstance[R, S, E]: MIOAsyncInstance[R, S, E] = new MIOAsyncInstance[R, S, E]

  class MIOAsyncInstance[R, S, E]
      extends cats.Defer[MIO_3[R, S, S, E, *]] with StackSafeMonad[MIO_3[R, S, S, E, *]] with Async[MIO_3[R, S, S, E, *]] {
    def suspend[A](fa: => MIO_3[R, S, S, E, A]): MIO_3[R, S, S, E, A]                             = MIO_3.defer(fa)
    def flatMap[A, B](fa: MIO_3[R, S, S, E, A])(f: A => MIO_3[R, S, S, E, B]): MIO_3[R, S, S, E, B] = fa.flatMap(f)
    def pure[A](x: A): MIO_3[R, S, S, E, A]                                                     = MIO_3.pure(x)

    def bracketCase[A, B](acquire: MIO_3[R, S, S, E, A])(use: A => MIO_3[R, S, S, E, B])(
        release: (A, ExitCase[Throwable]) => MIO_3[R, S, S, E, Unit]): MIO_3[R, S, S, E, B] =
      (acquire, info[R, S]).tupled.flatMap {
        case (a, (r, s, ec)) =>
          Catch(
            e => run(release(a, ExitCase.Error(e)), r, s, Callback.empty)(ec),
            use(a).cont(
              b => release(a, ExitCase.Completed) as b,
              e => release(a, ExitCase.Error(MIOExcept(e))) *> raise(e)
            )
          )
      }

    def raiseError[A](e: Throwable): MIO_3[R, S, S, E, A] = delay(throw e)

    def handleErrorWith[A](fa: MIO_3[R, S, S, E, A])(f: Throwable => MIO_3[R, S, S, E, A]): MIO_3[R, S, S, E, A] =
      fa.handleWith(e => f(MIOExcept(e)))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): MIO_3[R, S, S, E, A] =
      get[S] flatMap (s =>
        Await[R, S, E, A](cb =>
          k {
            case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
            case Left(exc)             => cb.broken(exc)
            case Right(value)          => cb.completed(s, value)
        }))

    def asyncF[A](k: (Either[Throwable, A] => Unit) => MIO_3[R, S, S, E, Unit]): MIO_3[R, S, S, E, A] =
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
