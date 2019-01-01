package manatki.data
import cats.data._
import cats.kernel.Semigroup
import cats.mtl.MonadState
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.{Applicative, FlatMap, Functor, Monad, MonadError, Monoid}
import manatki.data.RailT.{Exempt, Load, Transport}

/** a hybrid of StateT and EitherT */
final class RailT[F[_], S, A](val runF: F[S => F[Transport[S, A]]]) extends AnyVal {
  def run(s: S)(implicit F: FlatMap[F]): F[(S, Option[A])] = runT(s).map {
    case Exempt(s1)  => (s1, None)
    case Load(s1, a) => (s1, Some(a))
  }

  def runT(s: S)(implicit F: FlatMap[F]): F[Transport[S, A]] = runF.flatMap(_(s))
}

object RailT {

  sealed trait Transport[S, +A]
  final case class Exempt[S](s: S)        extends Transport[S, Nothing]
  final case class Load[S, A](s: S, a: A) extends Transport[S, A]

  def applyF[F[_], S, A](runF: F[S => F[Transport[S, A]]]): RailT[F, S, A] = new RailT(runF)

  def apply[F[_]] = new ApplyBuilder[F](true)

  class ApplyBuilder[F[_]](val __ : Boolean) extends AnyVal {
    def apply[S, A](f: S => F[Transport[S, A]])(implicit F: Applicative[F]): RailT[F, S, A] = applyF(F.pure(f))
  }
  def pure[F[_], S] = new PureBuilder[F, S](true)

  class PureBuilder[F[_], S](val __ : Boolean) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): RailT[F, S, A] = RailT[F](s => F.pure(Load(s, a)))
  }
  def inspect[F[_], S] = new InspectBuilder[F, S](true)

  class InspectBuilder[F[_], S](val __ : Boolean) extends AnyVal {
    def apply[A](f: S => A)(implicit F: Applicative[F]): RailT[F, S, A] = RailT[F](s => pureLoad[F](s, f(s)))
  }
  def modify[F[_]: Applicative, S](f: S => S): RailT[F, S, Unit] = RailT[F](s => pureLoad[F](f(s), ()))
  def set[F[_]: Applicative, S](s: S): RailT[F, S, Unit]         = RailT[F](_ => pureLoad[F](s, ()))
  def get[F[_]: Applicative, S]: RailT[F, S, S]                  = RailT[F](s => pureLoad[F](s, s))

  def fromWriterT[F[_]: Applicative, W: Semigroup, A](wr: WriterT[F, W, A]): RailT[F, W, A] =
    RailT[F](s => wr.run.map { case (w, a) => Load(s |+| w, a) })

  def fromReaderT[F[_]: Applicative, R, A](rd: ReaderT[F, R, A]): RailT[F, R, A] =
    RailT[F](r => rd.run(r).map(Load(r, _)))

  def fromEitherT[F[_]: Applicative, E, A](et: EitherT[F, E, A]): RailT[F, E, A] =
    RailT[F](e => et.value.map(_.fold(Exempt(_), Load(e, _))))

  def fromStateT[F[_]: Applicative, S, A](st: StateT[F, S, A]): RailT[F, S, A] =
    applyF(st.runF.map(run => s => run(s).map { case (s1, a) => Load(s1, a) }))

  def fromIorT[F[_]: Applicative, X: Semigroup, A](st: IorT[F, X, A]): RailT[F, X, A] =
    RailT[F](x =>
      st.value.map {
        case Ior.Left(x1)    => Exempt(x |+| x1)
        case Ior.Both(x1, a) => Load(x |+| x1, a)
        case Ior.Right(a)    => Load(x, a)
    })

  def pureLoad[F[_]] = new PureLoadBuilder[F]
  class PureLoadBuilder[F[_]](val __ : Boolean = true) {
    def apply[S, A](s: S, a: A)(implicit F: Applicative[F]): F[Transport[S, A]] = F.pure(Load(s, a))
  }

  implicit def railMonad[F[_], S](implicit F: Monad[F]): MonadError[RailT[F, S, ?], S] with MonadState[RailT[F, S, ?], S] =
    new MonadError[RailT[F, S, ?], S] with MonadState[RailT[F, S, ?], S] {
      def pure[A](x: A): RailT[F, S, A] = RailT[F](s => pureLoad[F](s, x))
      def flatMap[A, B](fa: RailT[F, S, A])(f: A => RailT[F, S, B]): RailT[F, S, B] =
        applyF(fa.runF.map { runa => s =>
          runa(s).flatMap {
            case ex @ Exempt(_) => F.pure(ex)
            case Load(s1, a)    => f(a).runT(s1)
          }
        })
      def raiseError[A](e: S): RailT[F, S, A] = RailT[F](_ => F.pure(Exempt(e)))
      def handleErrorWith[A](fa: RailT[F, S, A])(f: S => RailT[F, S, A]): RailT[F, S, A] =
        applyF(fa.runF.map { run => s =>
          run(s).flatMap {
            case l @ Load(_, _) => F.pure(l)
            case Exempt(s1)     => f(s1).runT(s1)
          }
        })

      def tailRecM[A, B](a: A)(f: A => RailT[F, S, Either[A, B]]): RailT[F, S, B] =
        RailT[F](s =>
          F.tailRecM((a, s)) {
            case (a1, s1) =>
              f(a1).runT(s1).map {
                case Exempt(s2)         => Right(Exempt(s2))
                case Load(s2, Left(a2)) => Left((a2, s2))
                case Load(s2, Right(b)) => Right(Load(s2, b))
              }

        })
      val monad                                 = this
      def get: RailT[F, S, S]                   = RailT.get
      def set(s: S): RailT[F, S, Unit]          = RailT.set(s)
      def inspect[A](f: S => A): RailT[F, S, A] = RailT.inspect(f)
      def modify(f: S => S): RailT[F, S, Unit]  = RailT.modify(f)
    }
}
