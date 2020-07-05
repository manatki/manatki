package manatki.data.eval

import cats.effect.{Async, ExitCase, Sync}
import cats.kernel.Monoid
import cats.{MonadError, StackSafeMonad}

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise, ExecutionContext => EC}
import scala.util.control.NonFatal
import cats.syntax.functor._
import cats.syntax.apply._

sealed trait MIO_1[-R, S, +E, +A] {
  final def run(r: R, init: S)(implicit ec: EC): Future[(S, Either[E, A])] = {
    val p = Promise[(S, Either[E, A])]()
    val cb = new MIO_1.Callback[S, E, A] {
      def raised(state: S, error: E): Unit    = p.success(state -> Left(error))
      def completed(state: S, value: A): Unit = p.success(state -> Right(value))
      def broken(e: Throwable): Unit           = p.failure(e)
    }
    MIO_1.run[R, S, E, A](this, r, init, cb)
    p.future
  }
  final def runEmpty(r: R)(implicit S: Monoid[S], ec: EC)               = run(r, Monoid.empty[S])
  final def runEmptyUnit(implicit ev: Unit <:< R, S: Monoid[S], ec: EC) = runEmpty(())
  final def runUnit(init: S)(implicit ev: Unit <:< R, ec: EC)           = run((), init)
}

object MIO_1 {
  def pure[S, A](a: A): MIO_1[Any, S, Nothing, A]        = Pure(a)
  def read[R, S]: MIO_1[R, S, Nothing, R]                = Read.asInstanceOf[MIO_1[R, S, Nothing, R]]
  def get[S]: MIO_1[Any, S, Nothing, S]                  = Get.asInstanceOf[MIO_1[Any, S, Nothing, S]]
  def set[S](s: S): MIO_1[Any, S, Nothing, Unit]         = Set(s)
  def update[S](f: S => S): MIO_1[Any, S, Nothing, Unit] = get[S].flatMap(s => set(f(s)))
  def raise[S, E](e: E): MIO_1[Any, S, E, Nothing]       = Raise(e)
  def defer[R, S, E, A](x: => MIO_1[R, S, E, A])         = Defer(() => x)
  def delay[S, A](x: => A): MIO_1[Any, S, Nothing, A]    = defer(pure(x))
  def exec[S]: MIO_1[Any, S, Nothing, EC]                = Exec.asInstanceOf[MIO_1[Any, S, Nothing, EC]]

  def write[S](s: S)(implicit S: Monoid[S]): MIO_1[Any, S, Nothing, Unit] = update(S.combine(_, s))

  type MIOGetter = MIO_1[Any, Nothing, Nothing, Nothing]
  final case class Pure[S, A](a: A)                            extends MIO_1[Any, S, Nothing, A]
  private case object Read                                             extends MIOGetter
  private case object Get                                              extends MIOGetter
  final case class Set[S](s: S)                                extends MIO_1[Any, S, Nothing, Unit]
  final case class Raise[S, E](e: E)                           extends MIO_1[Any, S, E, Nothing]
  final case class Defer[R, S, E, A](e: () => MIO_1[R, S, E, A]) extends MIO_1[R, S, E, A]
  private case object Exec                                             extends MIOGetter
  final case class Cont[R, S, E, A, B](
      src: MIO_1[R, S, E, A],
      ksuc: A => MIO_1[R, S, E, B],
      kerr: E => MIO_1[R, S, E, B]
  ) extends MIO_1[R, S, E, B]
  final case class Await[R, S, E, A](kont: Callback[S, E, A] => Unit) extends MIO_1[R, S, E, A]

  implicit class invariantOps[R, S, E, A](val calc: MIO_1[R, S, E, A]) extends AnyVal {
    def cont[B](f: A => MIO_1[R, S, E, B], h: E => MIO_1[R, S, E, B]): MIO_1[R, S, E, B] = Cont(calc, f, h)
    def flatMap[B](f: A => MIO_1[R, S, E, B]): MIO_1[R, S, E, B]                       = cont(f, raise(_: E))
    def handleWith(f: E => MIO_1[R, S, E, A]): MIO_1[R, S, E, A]                       = cont(pure(_: A), f)
    def handle(f: E => A): MIO_1[R, S, E, A]                                         = handleWith(e => pure(f(e)))
    def map[B](f: A => B): MIO_1[R, S, E, B]                                         = flatMap(a => pure(f(a)))
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

  def run[R, S, E, A](calc: MIO_1[R, S, E, A], r: R, init: S, cb: Callback[S, E, A])(implicit ec: EC): Unit = {
    def ret(x: Any): A = x.asInstanceOf[A]
    @tailrec def loop(c: MIO_1[R, S, E, A], s: S): Unit = c match {
      case Pure(a)     => cb.completed(s, a)
      case Read        => cb.completed(s, ret(r))
      case Get         => cb.completed(s, ret(s))
      case set: Set[S] => cb.completed(set.s, ret(()))
      case Raise(e)    => cb.raised(s.asInstanceOf[S], e)
      case Defer(f)    => loop(f(), s)
      case Await(f)    => f.asInstanceOf[Callback[S, E, A] => Unit](cb)
      case Exec        => cb.completed(s, ret(ec))
      case Cont(src, ks, ke) =>
        val kee = ke.asInstanceOf[E => MIO_1[R, S, E, A]]
        src match {
          case Pure(a)     => loop(ks(a), s)
          case Read        => loop(ks(ret(r)), s)
          case Get         => loop(ks(ret(r)), s)
          case set: Set[S] => loop(ks(ret(())), set.s)
          case Raise(e)    => loop(kee(e), s)
          case Defer(f)    => loop(f().cont(ks, kee), s)
          case Exec        => loop(ks(ret(ec)), s)
          case Cont(src1, ks1, ke1) =>
            val kee1 = ke1.asInstanceOf[E => MIO_1[R, S, E, A]]
            loop(src1.cont(a => ks1(a).cont(ks, kee), e => kee1(e).cont(ks, kee)), s)
          case Await(f) =>
            f.asInstanceOf[Callback[S, E, A] => Unit](new Callback[S, E, A] {
              def raised(state: S, error: E): Unit    = ec.execute(() => run(kee(error), r, state, cb))
              def completed(state: S, value: A): Unit = ec.execute(() => run(ks(value), r, state, cb))
              def broken(e: Throwable): Unit          = cb.broken(e)
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

  class MIOAsyncInstance[R, S, E]
      extends cats.Defer[MIO_1[R, S, E, *]] with StackSafeMonad[MIO_1[R, S, E, *]] with Async[MIO_1[R, S, E, *]] {
    def suspend[A](fa: => MIO_1[R, S, E, A]): MIO_1[R, S, E, A]                          = MIO_1.defer(fa)
    def flatMap[A, B](fa: MIO_1[R, S, E, A])(f: A => MIO_1[R, S, E, B]): MIO_1[R, S, E, B] = fa.flatMap(f)
    def pure[A](x: A): MIO_1[R, S, E, A]                                               = MIO_1.pure(x)

    def bracketCase[A, B](acquire: MIO_1[R, S, E, A])(use: A => MIO_1[R, S, E, B])(
        release: (A, ExitCase[Throwable]) => MIO_1[R, S, E, Unit]): MIO_1[R, S, E, B] =
      acquire.flatMap(
        a =>
          use(a).cont(
            b => release(a, ExitCase.Completed) as b,
            e => release(a, ExitCase.Error(MIOExcept(e))) *> raise(e)
        ))

    def raiseError[A](e: Throwable): MIO_1[R, S, E, A] = delay(throw e)

    def handleErrorWith[A](fa: MIO_1[R, S, E, A])(f: Throwable => MIO_1[R, S, E, A]): MIO_1[R, S, E, A] =
      fa.handleWith(e => f(MIOExcept(e)))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): MIO_1[R, S, E, A] =
      (get[S]: MIO_1[R, S, E, S]) flatMap (s =>
        Await[R, S, E, A](cb =>
          k {
            case Left(MIOExcept(e: E @unchecked)) => cb.raised(s, e)
            case Left(exc)             => cb.broken(exc)
            case Right(value)          => cb.completed(s, value)
        }))

    def asyncF[A](k: (Either[Throwable, A] => Unit) => MIO_1[R, S, E, Unit]): MIO_1[R, S, E, A] =
      ((get[S], exec[S], read[R, S]).tupled: MIO_1[R, S, E, (S, EC, R)]).flatMap {
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
