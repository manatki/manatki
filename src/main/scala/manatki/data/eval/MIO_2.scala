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

sealed trait MIO_2[-R, -S1, +S2, +E, +A] {
  final def run(r: R, init: S1)(implicit ec: EC): Future[(S2, Either[E, A])] = {
    val p = Promise[(S2, Either[E, A])]()
    val cb = new MIO_2.Callback[S2, E, A] {
      def raised(state: S2, error: E): Unit    = Future.successful((state, Left(error)))
      def completed(state: S2, value: A): Unit = Future.successful((state, Right(value)))
      def broken(e: Throwable): Unit           = Future.failed(e)
    }
    MIO_2.run[R, S1, S2, E, A](this, r, init, cb)
    p.future
  }
  final def runUnit(init: S1)(implicit ev: Unit <:< R, ec: EC) = run((), init)
}

object MIO_2 {
  def pure[S, A](a: A): MIO_2[Any, S, S, Nothing, A]                 = Pure(a)
  def read[R, S]: MIO_2[R, S, S, Nothing, R]                         = Read()
  def get[S]: MIO_2[Any, S, S, Nothing, S]                           = Get()
  def set[S](s: S): MIO_2[Any, Any, S, Nothing, Unit]                = Set(s)
  def update[S1, S2](f: S1 => S2): MIO_2[Any, S1, S2, Nothing, Unit] = get[S1].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): MIO_2[Any, S, S, E, Nothing]                = Raise(e)
  def defer[R, S1, S2, E, A](x: => MIO_2[R, S1, S2, E, A])           = Defer(() => x)
  def delay[S, A](x: => A): MIO_2[Any, S, S, Nothing, A]             = defer(pure(x))
  def exec[S]: MIO_2[Any, S, S, Nothing, EC]                         = Exec()

  def write[S](s: S)(implicit S: Monoid[S]): MIO_2[Any, S, S, Nothing, Unit] = update(S.combine(_, s))

  type MIOGetter = MIO_2[Any, Any, Nothing, Nothing, Nothing]

  sealed trait MIO2Stateless[-R, -S1, +S2, +E, A] extends MIO_2[R, S1, S2, E, A] {
    def stateAs: S1 As S2
    def coerce(s: S1): S2 = stateAs.coerce(s)
  }
  sealed trait MIOSS[-R, S, +E, A] extends MIO2Stateless[R, S, S, E, A] {
    def stateAs = As.refl[S]
  }

  final case class Pure[S, A](a: A)                                      extends MIOSS[Any, S, Nothing, A]
  final case class Read[S, R]()                                          extends MIOSS[R, S, Nothing, R]
  final case class Get[S]()                                              extends MIOSS[Any, S, Nothing, S]
  final case class Set[S](s: S)                                          extends MIO_2[Any, Any, S, Nothing, Unit]
  final case class Raise[S, E](e: E)                                     extends MIOSS[Any, S, E, Nothing]
  final case class Defer[R, S1, S2, E, A](e: () => MIO_2[R, S1, S2, E, A]) extends MIO_2[R, S1, S2, E, A]
  final case class Exec[S]()                                             extends MIOSS[Any, S, Nothing, EC]
  final case class Cont[R, S1, S2, S3, E1, E2, A, B](
      src: MIO_2[R, S1, S2, E1, A],
      ksuc: A => MIO_2[R, S2, S3, E2, B],
      kerr: E1 => MIO_2[R, S2, S3, E2, B]
  ) extends MIO_2[R, S1, S3, E2, B] {
    type MidState = S2
    type MidErr   = E1
    type MidVal   = A
  }
  final case class Await[R, S, E, A](kont: Callback[S, E, A] => Unit) extends MIO_2[R, Any, S, E, A]

  implicit class invariantOps[R, S1, S2, E, A](val calc: MIO_2[R, S1, S2, E, A]) extends AnyVal {
    def cont[E2, S3, B](f: A => MIO_2[R, S2, S3, E2, B], h: E => MIO_2[R, S2, S3, E2, B]): MIO_2[R, S1, S3, E2, B] =
      Cont(calc, f, h)
    def flatMap[B](f: A => MIO_2[R, S2, S2, E, B]): MIO_2[R, S1, S2, E, B]       = cont(f, raise(_: E))
    def handleWith[E2](f: E => MIO_2[R, S2, S2, E2, A]): MIO_2[R, S1, S2, E2, A] = cont(pure(_: A), f)
    def handle(f: E => A): MIO_2[R, S1, S2, E, A]                              = handleWith(e => pure(f(e)))
    def map[B](f: A => B): MIO_2[R, S1, S2, E, B]                              = flatMap(a => pure(f(a)))
  }

  implicit class successfulOps[R, S1, S2, A](val calc: MIO_2[R, S1, S2, Nothing, A]) extends AnyVal {
    def flatMapS[S3, E, B](f: A => MIO_2[R, S2, S3, E, B]): MIO_2[R, S1, S3, E, B] =
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

  def run[R, S1, S2, E, A](calc: MIO_2[R, S1, S2, E, A], r: R, init: S1, cb: Callback[S2, E, A])(
      implicit ec: EC): Unit = {
    def ret(x: Any): A = x.asInstanceOf[A]
    @tailrec def loop[S](c: MIO_2[R, S, S2, E, A], s: S): Unit = c match {
      case stateless: MIO2Stateless[R, S, S2, E, A] =>
        import stateless.coerce
        stateless match {
          case Pure(a)  => cb.completed(coerce(s), a)
          case Read()   => cb.completed(coerce(s), ret(r))
          case Get()    => cb.completed(coerce(s), ret(s))
          case Exec()   => cb.completed(coerce(s), ret(ec))
          case Raise(e) => cb.raised(coerce(s), e)
        }

      case Set(s1)  => cb.completed(s1, ret(()))
      case Defer(f) => loop(f(), s)
      case Await(f) => f(cb)
      case c @ Cont(src, ks, ke) =>
        src match {
          case Pure(a)  => loop(ks(a), s)
          case Read()   => loop(ks(ret(r)), s)
          case Get()    => loop(ks(ret(r)), s)
          case Set(s1)  => loop(ks(ret(())), s1)
          case Raise(e) => loop(ke(e), s)
          case Defer(f) => loop(f().cont(ks, ke), s)
          case Exec()   => loop(ks(ret(ec)), s)
          case c2 @ Cont(src1, ks1, ke1) =>
            loop(src1.cont(a => ks1(a).cont(ks, ke), e => ke1(e).cont(ks, ke)), s)
          case Await(f) =>
            f(new Callback[c.MidState, c.MidErr, c.MidVal] {
              def raised(state: c.MidState, error: c.MidErr): Unit    = ec.execute(() => run(ke(error), r, state, cb))
              def completed(state: c.MidState, value: c.MidVal): Unit = ec.execute(() => run(ks(value), r, state, cb))
              def broken(e: Throwable): Unit                          = cb.broken(e)
            })
        }
    }
    try {
      loop(calc, init)
    } catch {
      case NonFatal(e) => cb.broken(e)
    }
  }

  implicit def calcInstance[R, S, E]: MIOAsyncInstance[R, S, E] = new MIOAsyncInstance[R, S, E]
//
  class MIOAsyncInstance[R, S, E]
      extends cats.Defer[MIO_2[R, S, S, E, *]] with StackSafeMonad[MIO_2[R, S, S, E, *]] with Async[MIO_2[R, S, S, E, *]] {
    def suspend[A](fa: => MIO_2[R, S, S, E, A]): MIO_2[R, S, S, E, A]                             = MIO_2.defer(fa)
    def flatMap[A, B](fa: MIO_2[R, S, S, E, A])(f: A => MIO_2[R, S, S, E, B]): MIO_2[R, S, S, E, B] = fa.flatMap(f)
    def pure[A](x: A): MIO_2[R, S, S, E, A]                                                     = MIO_2.pure(x)

    def bracketCase[A, B](acquire: MIO_2[R, S, S, E, A])(use: A => MIO_2[R, S, S, E, B])(
        release: (A, ExitCase[Throwable]) => MIO_2[R, S, S, E, Unit]): MIO_2[R, S, S, E, B] =
      acquire.flatMap(
        a =>
          use(a).cont(
            b => release(a, ExitCase.Completed) as b,
            e => release(a, ExitCase.Error(MIOExcept(e))) *> raise(e)
        ))

    def raiseError[A](e: Throwable): MIO_2[R, S, S, E, A] = delay(throw e)

    def handleErrorWith[A](fa: MIO_2[R, S, S, E, A])(f: Throwable => MIO_2[R, S, S, E, A]): MIO_2[R, S, S, E, A] =
      fa.handleWith(e => f(MIOExcept(e)))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): MIO_2[R, S, S, E, A] =
      (get[S]: MIO_2[R, S, S, E, S]) flatMap (s =>
        Await[R, S, E, A](cb =>
          k {
            case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
            case Left(exc)             => cb.broken(exc)
            case Right(value)          => cb.completed(s, value)
        }))

    def asyncF[A](k: (Either[Throwable, A] => Unit) => MIO_2[R, S, S, E, Unit]): MIO_2[R, S, S, E, A] =
      ((get[S], exec[S], read[R, S]).tupled: MIO_2[R, S, S, E, (S, EC, R)]).flatMap {
        case (s, ec, r) =>
          implicit val eci = ec
          Await[R, S, E, A] { cb =>
            val mio = k {
              case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
              case Left(exc)             => cb.broken(exc)
              case Right(value)          => cb.completed(s, value)
            }
            run(mio, r, s, Callback.empty)
          }
      }
  }

  final case class MIOExcept[E](e: E) extends Throwable
}
