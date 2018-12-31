package manatki.data
import cats.mtl.MonadState
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad, MonadError}

/** a hybrid of StateT and EitherT */
final case class RailT[F[_], S, A](run: S => F[(S, Option[A])])

object RailT {

  def pure[F[_], S] = new PureBuilder[F, S](true)
  class PureBuilder[F[_], S](val __ : Boolean) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): RailT[F, S, A] = RailT(s => F.pure((s, Some(a))))
  }

  def inspect[F[_], S] = new InspectBuilder[F, S](true)
  class InspectBuilder[F[_], S](val __ : Boolean) extends AnyVal {
    def apply[A](f: S => A)(implicit F: Applicative[F]): RailT[F, S, A] = RailT(s => F.pure((s, Some(f(s)))))
  }

  def modify[F[_], S](f: S => S)(implicit F: Applicative[F]): RailT[F, S, Unit] = RailT(s => F.pure((s, Some(()))))
  def set[F[_], S](s: S)(implicit F: Applicative[F]): RailT[F, S, Unit] = RailT(s => F.pure((s, Some(()))))
  def get[F[_] : Applicative, S](implicit F: Applicative[F]): RailT[F, S, S] = RailT(s => F.pure((s, Some(s))))


  implicit def railMonad[F[_], S](implicit F: Monad[F]): MonadError[RailT[F, S, ?], S] with MonadState[RailT[F, S, ?], S] =
    new MonadError[RailT[F, S, ?], S] with MonadState[RailT[F, S, ?], S] {
      def pure[A](x: A): RailT[F, S, A] = RailT(s => F.pure((s, Some(x))))
      def flatMap[A, B](fa: RailT[F, S, A])(f: A => RailT[F, S, B]): RailT[F, S, B] =
        RailT(s =>
          fa.run(s).flatMap {
            case (s1, None) => F.pure((s1, None))
            case (s1, Some(x)) => f(x).run(s1)
          })
      def raiseError[A](e: S): RailT[F, S, A] = RailT(_ => F.pure((e, None)))
      def handleErrorWith[A](fa: RailT[F, S, A])(f: S => RailT[F, S, A]): RailT[F, S, A] =
        RailT(s =>
          fa.run(s).flatMap {
            case p@(_, Some(_)) => F.pure(p)
            case (s1, None) => f(s1).run(s1)
          })
      def tailRecM[A, B](a: A)(f: A => RailT[F, S, Either[A, B]]): RailT[F, S, B] =
        RailT(s =>
          F.tailRecM((a, s)) {
            case (a1, s1) =>
              f(a1).run(s1).map {
                case (s2, None) => Right((s2, None))
                case (s2, Some(Left(a2))) => Left((a2, s2))
                case (s2, Some(Right(b))) => Right((s2, Some(b)))
              }

          })
      val monad = this
      def get: RailT[F, S, S] = RailT.get
      def set(s: S): RailT[F, S, Unit] = RailT.set(s)
      def inspect[A](f: S => A): RailT[F, S, A] = RailT.inspect(f)
      def modify(f: S => S): RailT[F, S, Unit] = RailT.modify(f)
    }


}
