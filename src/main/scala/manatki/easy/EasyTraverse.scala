package manatki.easy
import cats.{Applicative, Apply, Eval, Traverse}
import cats.syntax.apply._
import cats.syntax.functor._
import EasyTraverse._

trait EasyTraverse[F[_]] extends Traverse[F] {
  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    traverse[Folder[B, ?], A, B](fa)(a => Eval.now(b => b.map(f(_, a))))(FolderLeftApplicative.as[B]).value(Eval.now(b)).value
  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    traverse[Folder[B, ?], A, B](fa)(a => Eval.now(b => f(a, b)))(FolderRightApplicative.as[B]).value(lb)
}

object EasyTraverse {
  type Folder[A, B] = Eval[Eval[A] => Eval[A]]

  private val EA = Apply[Eval]

  abstract class FolderApplicative extends Applicative[Folder[Any, ?]] {
    override def pure[A](x: A): Folder[Any, A] = Eval.now(identity)
    def comp[A](a: A => A, b: A => A): A => A

    override def ap[A, B](ff: Folder[Any, A => B])(fa: Folder[Any, A]): Folder[Any, B] =
      EA.map2(ff, fa)(comp[Eval[Any]])
    def as[T]: Applicative[Folder[T, ?]] = asInstanceOf[Applicative[Folder[T, ?]]]
  }


  object FolderLeftApplicative extends FolderApplicative {
    override def comp[A](a: A => A, b: A => A) = a andThen b
  }

  object FolderRightApplicative extends FolderApplicative {
    override def comp[A](a: A => A, b: A => A) = a compose b
  }
}
