package manatki.data.eval
import cats.{MonadError, StackSafeMonad}
import cats.kernel.Monoid
import cats.syntax.either._

sealed trait Calc[-R, S, +E, +A] {
  final def run(r: R, init: S): (S, Either[E, A]) = Calc.run(this, r, init)
  final def runEmpty(r: R)(implicit S: Monoid[S]) = run(r, Monoid.empty[S])
}

object Calc {
  def pure[S, A](a: A): Calc[Any, S, Nothing, A]        = Pure(a)
  def read[R, S]: Calc[R, S, Nothing, R]                = Read()
  def get[S]: Calc[Any, S, Nothing, S]                  = Get()
  def set[S](s: S): Calc[Any, S, Nothing, Unit]         = Set(s)
  def update[S](f: S => S): Calc[Any, S, Nothing, Unit] = get[S].flatMap(s => set(f(s)))
  def raise[S, E](e: E): Calc[Any, S, E, Nothing]       = Raise(e)
  def defer[R, S, E, A](x: => Calc[R, S, E, A])         = Defer(() => x)
  def delay[S, A](x: => A): Calc[Any, S, Nothing, A]    = defer(pure(x))

  def write[S](s: S)(implicit S: Monoid[S]): Calc[Any, S, Nothing, Unit] = update(S.combine(_, s))

  final case class Pure[S, A](a: A)                             extends Calc[Any, S, Nothing, A]
  final case class Read[R, S]()                                 extends Calc[R, S, Nothing, R]
  final case class Get[S]()                                     extends Calc[Any, S, Nothing, S]
  final case class Set[S](s: S)                                 extends Calc[Any, S, Nothing, Unit]
  final case class Raise[S, E](e: E)                            extends Calc[Any, S, E, Nothing]
  final case class Defer[R, S, E, A](e: () => Calc[R, S, E, A]) extends Calc[R, S, E, A]
  final case class Cont[R, S, E, A, B](
      src: Calc[R, S, E, A],
      ksuc: A => Calc[R, S, E, B],
      kerr: E => Calc[R, S, E, B]
  ) extends Calc[R, S, E, B]

  implicit class invariantOps[R, S, E, A](val calc: Calc[R, S, E, A]) extends AnyVal {
    def cont[B](f: A => Calc[R, S, E, B], h: E => Calc[R, S, E, B]): Calc[R, S, E, B] = Cont(calc, f, h)
    def flatMap[B](f: A => Calc[R, S, E, B]): Calc[R, S, E, B]                        = cont(f, raise(_: E))
    def handleWith(f: E => Calc[R, S, E, A]): Calc[R, S, E, A]                        = cont(pure(_: A), f)
    def handle(f: E => A): Calc[R, S, E, A]                                           = handleWith(e => pure(f(e)))
    def map[B](f: A => B): Calc[R, S, E, B]                                           = flatMap(a => pure(f(a)))
  }

  def run[R, S, E, A](calc: Calc[R, S, E, A], r: R, init: S): (S, Either[E, A]) =
    calc match {
      case Pure(a)     => (init.asInstanceOf[S], Right(a))
      case Read()      => (init.asInstanceOf[S], Right(r.asInstanceOf[A]))
      case Get()       => (init.asInstanceOf[S], Right(init.asInstanceOf[A]))
      case set: Set[S] => (set.s, Right(().asInstanceOf[A]))
      case Raise(e)    => (init.asInstanceOf[S], Left(e))
      case Defer(f)    => run(f(), r, init)
      case Cont(src, ks, ke) =>
        val kee = ke.asInstanceOf[E => Calc[R, S, E, A]]
        src match {
          case Pure(a)     => run(ks(a), r, init)
          case Read()      => run(ks(r.asInstanceOf[A]), r, init)
          case Get()       => run(ks(init.asInstanceOf[A]), r, init)
          case set: Set[S] => run(ks(().asInstanceOf[A]), r, set.s)
          case Raise(e)    => run(kee(e), r, init)
          case Defer(f)    => run(f().cont(ks, kee), r, init)
          case Cont(src1, ks1, ke1) =>
            val kee1 = ke1.asInstanceOf[E => Calc[R, S, E, A]]
            run(src1.cont(a => ks1(a).cont(ks, kee), e => kee1(e).cont(ks, kee)), r, init)
        }
    }

  implicit def calcInstance[R, S, E]: CalcFunctorInstance[R, S, E] = new CalcFunctorInstance[R, S, E]

  class CalcFunctorInstance[R, S, E]
      extends MonadError[Calc[R, S, E, ?], E] with cats.Defer[Calc[R, S, E, ?]] with StackSafeMonad[Calc[R, S, E, ?]] {
    def defer[A](fa: => Calc[R, S, E, A]): Calc[R, S, E, A]                                  = Calc.defer(fa)
    def raiseError[A](e: E): Calc[R, S, E, A]                                                = Calc.raise(e)
    def handleErrorWith[A](fa: Calc[R, S, E, A])(f: E => Calc[R, S, E, A]): Calc[R, S, E, A] = fa.handleWith(f)
    def flatMap[A, B](fa: Calc[R, S, E, A])(f: A => Calc[R, S, E, B]): Calc[R, S, E, B]      = fa.flatMap(f)
    def pure[A](x: A): Calc[R, S, E, A]                                                      = Calc.pure(x)
  }
}
