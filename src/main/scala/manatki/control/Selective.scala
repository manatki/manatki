package manatki.control
import cats.Applicative
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.either._

trait Selective[F[_]] extends Applicative[F] {
  def select[A, B](fab: F[Either[A, B]], fa: => F[B]): F[B]

  def ifS[A](fb: F[Boolean], th: => F[A], el: => F[A]): F[A] =
    select(
      select[Unit, Either[Unit, A]](
        map(fb)(b => if (b) ().asLeft else ().asLeft.asRight),
        map(th)(_.asRight)
      ),
      el
    )
}
