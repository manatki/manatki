package manatki.data.eval

import cats.kernel.Monoid
import cats.syntax.either._
import cats.{MonadError, StackSafeMonad}

import scala.concurrent.{ExecutionContext, Future, Promise}

sealed trait MIO[-R, S, +E, +A] {
  final def run(r: R, init: S)(implicit ec: ExecutionContext): Future[(S, Either[E, A])] = MIO.run(this, r, init)
  final def runEmpty(r: R)(implicit S: Monoid[S], ec: ExecutionContext)                  = run(r, Monoid.empty[S])
  final def runEmptyUnit(implicit ev: Unit <:< R, S: Monoid[S], ec: ExecutionContext)    = runEmpty(())
  final def runUnit(init: S)(implicit ev: Unit <:< R, ec: ExecutionContext)              = run((), init)
}

object MIO {
  def pure[S, A](a: A): MIO[Any, S, Nothing, A]        = Pure(a)
  def read[R, S]: MIO[R, S, Nothing, R]                = Read()
  def get[S]: MIO[Any, S, Nothing, S]                  = Get()
  def set[S](s: S): MIO[Any, S, Nothing, Unit]         = Set(s)
  def update[S](f: S => S): MIO[Any, S, Nothing, Unit] = get[S].flatMap(s => set(f(s)))
  def raise[S, E](e: E): MIO[Any, S, E, Nothing]       = Raise(e)
  def defer[R, S, E, A](x: => MIO[R, S, E, A])         = Defer(() => x)
  def delay[S, A](x: => A): MIO[Any, S, Nothing, A]    = defer(pure(x))

  def write[S](s: S)(implicit S: Monoid[S]): MIO[Any, S, Nothing, Unit] = update(S.combine(_, s))

  final case class Pure[S, A](a: A)                            extends MIO[Any, S, Nothing, A]
  final case class Read[R, S]()                                extends MIO[R, S, Nothing, R]
  final case class Get[S]()                                    extends MIO[Any, S, Nothing, S]
  final case class Set[S](s: S)                                extends MIO[Any, S, Nothing, Unit]
  final case class Raise[S, E](e: E)                           extends MIO[Any, S, E, Nothing]
  final case class Defer[R, S, E, A](e: () => MIO[R, S, E, A]) extends MIO[R, S, E, A]
  final case class Cont[R, S, E, A, B](
      src: MIO[R, S, E, A],
      ksuc: A => MIO[R, S, E, B],
      kerr: E => MIO[R, S, E, B]
  ) extends MIO[R, S, E, B]
  final case class Await[R, S, E, A](kont: ((Either[E, A], S) => Unit) => Unit) extends MIO[R, S, E, A]

  implicit class invariantOps[R, S, E, A](val calc: MIO[R, S, E, A]) extends AnyVal {
    def cont[B](f: A => MIO[R, S, E, B], h: E => MIO[R, S, E, B]): MIO[R, S, E, B] = Cont(calc, f, h)
    def flatMap[B](f: A => MIO[R, S, E, B]): MIO[R, S, E, B]                       = cont(f, raise(_: E))
    def handleWith(f: E => MIO[R, S, E, A]): MIO[R, S, E, A]                       = cont(pure(_: A), f)
    def handle(f: E => A): MIO[R, S, E, A]                                         = handleWith(e => pure(f(e)))
    def map[B](f: A => B): MIO[R, S, E, B]                                         = flatMap(a => pure(f(a)))
  }

  def run[R, S, E, A](calc: MIO[R, S, E, A], r: R, init: S)(implicit ec: ExecutionContext): Future[(S, Either[E, A])] = {
    import Future.{successful => now}

    calc match {
      case Pure(a)     => now(init.asInstanceOf[S], Right(a))
      case Read()      => now(init.asInstanceOf[S], Right(r.asInstanceOf[A]))
      case Get()       => now(init.asInstanceOf[S], Right(init.asInstanceOf[A]))
      case set: Set[S] => now(set.s, Right(().asInstanceOf[A]))
      case Raise(e)    => now(init.asInstanceOf[S], Left(e))
      case Defer(f)    => run(f(), r, init)
      case Await(f) =>
        val p = Promise[(S, Either[E, A])]
        f((e, s) => p.success((s, e)))
        p.future
      case Cont(src, ks, ke) =>
        val kee = ke.asInstanceOf[E => MIO[R, S, E, A]]
        src match {
          case Pure(a)     => run(ks(a), r, init)
          case Read()      => run(ks(r.asInstanceOf[A]), r, init)
          case Get()       => run(ks(init.asInstanceOf[A]), r, init)
          case set: Set[S] => run(ks(().asInstanceOf[A]), r, set.s)
          case Raise(e)    => run(kee(e), r, init)
          case Defer(f)    => run(f().cont(ks, kee), r, init)
          case Cont(src1, ks1, ke1) =>
            val kee1 = ke1.asInstanceOf[E => MIO[R, S, E, A]]
            run(src1.cont(a => ks1(a).cont(ks, kee), e => kee1(e).cont(ks, kee)), r, init)
          case Await(f) =>
            val p = Promise[(S, Either[E, A])]
            f {
              case (Left(e), s)  => run(kee(e), r, s).foreach(p.success)
              case (Right(a), s) => run(ks(a), r, s).foreach(p.success)
            }
            p.future
        }
    }
  }

  implicit def calcInstance[R, S, E]: CalcFunctorInstance[R, S, E] = new CalcFunctorInstance[R, S, E]

  class CalcFunctorInstance[R, S, E]
      extends MonadError[MIO[R, S, E, ?], E] with cats.Defer[MIO[R, S, E, ?]] with StackSafeMonad[MIO[R, S, E, ?]] {
    def defer[A](fa: => MIO[R, S, E, A]): MIO[R, S, E, A]                                 = MIO.defer(fa)
    def raiseError[A](e: E): MIO[R, S, E, A]                                              = MIO.raise(e)
    def handleErrorWith[A](fa: MIO[R, S, E, A])(f: E => MIO[R, S, E, A]): MIO[R, S, E, A] = fa.handleWith(f)
    def flatMap[A, B](fa: MIO[R, S, E, A])(f: A => MIO[R, S, E, B]): MIO[R, S, E, B]      = fa.flatMap(f)
    def pure[A](x: A): MIO[R, S, E, A]                                                    = MIO.pure(x)
  }
}
