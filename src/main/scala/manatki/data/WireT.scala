package manatki.data
import cats.data._
import cats.kernel.Semigroup
import cats.mtl.MonadState
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.{Applicative, FlatMap, Functor, Monad, MonadError, Monoid}
import manatki.data.WireT.{Vacant, Full, Socket}

/** a hybrid of StateT and EitherT */
final class WireT[F[_], S, A](val runF: F[S => F[Socket[S, A]]]) extends AnyVal {
  def run(s: S)(implicit F: FlatMap[F]): F[(S, Option[A])] = runT(s).map {
    case Vacant(s1)  => (s1, None)
    case Full(s1, a) => (s1, Some(a))
  }

  def runT(s: S)(implicit F: FlatMap[F]): F[Socket[S, A]] = runF.flatMap(_(s))
}

object WireT {

  sealed trait Socket[S, +A]
  final case class Vacant[S](s: S)        extends Socket[S, Nothing]
  final case class Full[S, A](s: S, a: A) extends Socket[S, A]

  def applyF[F[_], S, A](runF: F[S => F[Socket[S, A]]]): WireT[F, S, A] = new WireT(runF)

  def apply[F[_]] = new ApplyBuilder[F](true)

  class ApplyBuilder[F[_]](val __ : Boolean) extends AnyVal {
    def apply[S, A](f: S => F[Socket[S, A]])(implicit F: Applicative[F]): WireT[F, S, A] = applyF(F.pure(f))
  }
  def pure[F[_], S] = new PureBuilder[F, S](true)

  class PureBuilder[F[_], S](val __ : Boolean) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): WireT[F, S, A] = WireT[F](s => F.pure(Full(s, a)))
  }
  def inspect[F[_], S] = new InspectBuilder[F, S](true)

  class InspectBuilder[F[_], S](val __ : Boolean) extends AnyVal {
    def apply[A](f: S => A)(implicit F: Applicative[F]): WireT[F, S, A] = WireT[F](s => pureLoad[F](s, f(s)))
  }
  def modify[F[_]: Applicative, S](f: S => S): WireT[F, S, Unit] = WireT[F](s => pureLoad[F](f(s), ()))
  def set[F[_]: Applicative, S](s: S): WireT[F, S, Unit]         = WireT[F](_ => pureLoad[F](s, ()))
  def get[F[_]: Applicative, S]: WireT[F, S, S]                  = WireT[F](s => pureLoad[F](s, s))

  def fromWriterT[F[_]: Applicative, W: Semigroup, A](wr: WriterT[F, W, A]): WireT[F, W, A] =
    WireT[F](s => wr.run.map { case (w, a) => Full(s |+| w, a) })

  def fromReaderT[F[_]: Applicative, R, A](rd: ReaderT[F, R, A]): WireT[F, R, A] =
    WireT[F](r => rd.run(r).map(Full(r, _)))

  def fromEitherT[F[_]: Applicative, E, A](et: EitherT[F, E, A]): WireT[F, E, A] =
    WireT[F](e => et.value.map(_.fold(Vacant(_), Full(e, _))))

  def fromStateT[F[_]: Applicative, S, A](st: StateT[F, S, A]): WireT[F, S, A] =
    applyF(st.runF.map(run => s => run(s).map { case (s1, a) => Full(s1, a) }))

  def fromIorT[F[_]: Applicative, X: Semigroup, A](st: IorT[F, X, A]): WireT[F, X, A] =
    WireT[F](x =>
      st.value.map {
        case Ior.Left(x1)    => Vacant(x |+| x1)
        case Ior.Both(x1, a) => Full(x |+| x1, a)
        case Ior.Right(a)    => Full(x, a)
    })

  def pureLoad[F[_]] = new PureLoadBuilder[F]
  class PureLoadBuilder[F[_]](val __ : Boolean = true) {
    def apply[S, A](s: S, a: A)(implicit F: Applicative[F]): F[Socket[S, A]] = F.pure(Full(s, a))
  }

  implicit def wireMonad[F[_], S](implicit F: Monad[F]): MonadError[WireT[F, S, ?], S] with MonadState[WireT[F, S, ?], S] =
    new MonadError[WireT[F, S, ?], S] with MonadState[WireT[F, S, ?], S] {
      def pure[A](x: A): WireT[F, S, A] = WireT[F](s => pureLoad[F](s, x))
      def flatMap[A, B](fa: WireT[F, S, A])(f: A => WireT[F, S, B]): WireT[F, S, B] =
        applyF(fa.runF.map { runa => s =>
          runa(s).flatMap {
            case ex @ Vacant(_) => F.pure(ex)
            case Full(s1, a)    => f(a).runT(s1)
          }
        })
      def raiseError[A](e: S): WireT[F, S, A] = WireT[F](_ => F.pure(Vacant(e)))
      def handleErrorWith[A](fa: WireT[F, S, A])(f: S => WireT[F, S, A]): WireT[F, S, A] =
        applyF(fa.runF.map { run => s =>
          run(s).flatMap {
            case l @ Full(_, _) => F.pure(l)
            case Vacant(s1)     => f(s1).runT(s1)
          }
        })

      def tailRecM[A, B](a: A)(f: A => WireT[F, S, Either[A, B]]): WireT[F, S, B] =
        WireT[F](s =>
          F.tailRecM((a, s)) {
            case (a1, s1) =>
              f(a1).runT(s1).map {
                case Vacant(s2)         => Right(Vacant(s2))
                case Full(s2, Left(a2)) => Left((a2, s2))
                case Full(s2, Right(b)) => Right(Full(s2, b))
              }

        })
      val monad                                 = this
      def get: WireT[F, S, S]                   = WireT.get
      def set(s: S): WireT[F, S, Unit]          = WireT.set(s)
      def inspect[A](f: S => A): WireT[F, S, A] = WireT.inspect(f)
      def modify(f: S => S): WireT[F, S, Unit]  = WireT.modify(f)
    }
}
