package manatki.err

import manatki.err.proc._

trait LocalT[F[+_, +_], C[f[+_, +_]], +E, +A] { self =>
  def run(context: C[F])(implicit F: Proc[F]): F[E, A]

  def foldWith[X, B](
      h: E => LocalT[F, C, X, B],
      f: A => LocalT[F, C, X, B]
  )(implicit F: Proc[F]): LocalT[F, C, X, B] = new LocalT.FoldWith(self, h, f)

  def flatMap[E1 >: E, B](f: A => LocalT[F, C, E1, B])(implicit F: Proc[F]): LocalT[F, C, E1, B] =
    new LocalT.FlatMap(self, f)

  def handleWith[X, A1 >: A](f: E => LocalT[F, C, X, A1])(implicit F: Proc[F]): LocalT[F, C, X, A1] =
    new LocalT.HandleWith(self, f)
}

object LocalT {
  def pure[F[+_, +_], C[f[+_, +_]]] = new PureApp[F, C](true)

  def raise[F[+_, +_], C[f[+_, +_]]] = new RaiseApp[F, C](true)

  def access[F[+_, +_], C[f[+_, +_]]] = new AccessApp[F, C](true)

  def read[F[+_, +_], C[f[+_, +_]]]: LocalT[F, C, Nothing, C[F]] = new LocalSuccess[F, C, C[F]] {
    def run(context: C[F])(implicit F: Proc[F]): F[Nothing, C[F]] = F.pure(context)
  }

  class PureApp[F[+_, +_], C[f[+_, +_]]](private val __ : Boolean) extends AnyVal {
    def apply[A](a: A): LocalT[F, C, Nothing, A] = new Pure(a)
  }

  class RaiseApp[F[+_, +_], C[f[+_, +_]]](private val __ : Boolean) extends AnyVal {
    def apply[E](e: E): LocalT[F, C, E, Nothing] = new Raise(e)
  }

  class AccessApp[F[+_, +_], C[f[+_, +_]]](private val __ : Boolean) extends AnyVal {
    def apply[A](f: Access[F, C, A]): LocalT[F, C, Nothing, A] = f
  }

  trait Access[F[+_, +_], C[f[+_, +_]], A] extends LocalSuccess[F, C, A] {
    def get(context: C[F]): A

    def run(context: C[F])(implicit F: Proc[F]): F[Nothing, A] = F.pure(get(context))
  }

  abstract class FoldWithA[F[+_, +_], C[f[+_, +_]], E, A, +X, +B](
      self: LocalT[F, C, E, A],
  ) extends LocalT[F, C, X, B] { up =>

    def handler(e: E)(implicit F: Proc[F]): LocalT[F, C, X, B]
    def successor(a: A)(implicit F: Proc[F]): LocalT[F, C, X, B]
    def run(context: C[F])(implicit F: Proc[F]): F[X, B] =
      self.run(context).foldWith(handler(_).run(context), successor(_).run(context))

    override def foldWith[U, D](
        j: X => LocalT[F, C, U, D],
        g: B => LocalT[F, C, U, D]
    )(implicit F: Proc[F]): LocalT[F, C, U, D] =
      new FoldWithA[F, C, E, A, U, D](self) {
        def handler(e: E)(implicit F: Proc[F]): LocalT[F, C, U, D]   = up.handler(e).foldWith(j, g)
        def successor(a: A)(implicit F: Proc[F]): LocalT[F, C, U, D] = up.successor(a).foldWith(j, g)
      }

    override def flatMap[E1 >: X, D](f: B => LocalT[F, C, E1, D])(implicit F: Proc[F]): LocalT[F, C, E1, D] =
      new FoldWithA[F, C, E, A, E1, D](self) {
        def handler(e: E)(implicit F: Proc[F]): LocalT[F, C, E1, D]   = up.handler(e).flatMap(f)
        def successor(a: A)(implicit F: Proc[F]): LocalT[F, C, E1, D] = up.successor(a).flatMap(f)
      }

    override def handleWith[Y, B1 >: B](
        h: X => LocalT[F, C, Y, B1]
    )(implicit F: Proc[F]): LocalT[F, C, Y, B1] =
      new FoldWithA[F, C, E, A, Y, B1](self) {
        def handler(e: E)(implicit F: Proc[F]): LocalT[F, C, Y, B1]   = up.handler(e).handleWith(h)
        def successor(a: A)(implicit F: Proc[F]): LocalT[F, C, Y, B1] = up.successor(a).handleWith(h)
      }
  }

  private class FoldWith[F[+_, +_], C[f[+_, +_]], E, A, +X, +B](
      self: LocalT[F, C, E, A],
      h: E => LocalT[F, C, X, B],
      f: A => LocalT[F, C, X, B]
  ) extends FoldWithA[F, C, E, A, X, B](self) {
    def handler(e: E)(implicit F: Proc[F]): LocalT[F, C, X, B] = h(e)

    def successor(a: A)(implicit F: Proc[F]): LocalT[F, C, X, B] = f(a)
  }

  class FlatMap[F[+_, +_], C[f[+_, +_]], E, A, +B](
      self: LocalT[F, C, E, A],
      f: A => LocalT[F, C, E, B]
  ) extends FoldWithA[F, C, E, A, E, B](self) {
    def handler(e: E)(implicit F: Proc[F]): LocalT[F, C, E, B] = LocalT.raise[F, C](e)

    def successor(a: A)(implicit F: Proc[F]): LocalT[F, C, E, B] = f(a)

    override def run(context: C[F])(implicit F: Proc[F]): F[E, B] =
      self.run(context).flatMap(f(_).run(context))

    override def flatMap[E1 >: E, D](g: B => LocalT[F, C, E1, D])(implicit F: Proc[F]): LocalT[F, C, E1, D] =
      self.flatMap(f(_).flatMap(g))
  }

  class HandleWith[F[+_, +_], C[f[+_, +_]], E, A, +X](
      self: LocalT[F, C, E, A],
      h: E => LocalT[F, C, X, A]
  ) extends FoldWithA[F, C, E, A, X, A](self) {
    def handler(e: E)(implicit F: Proc[F]): LocalT[F, C, X, A] = h(e)

    def successor(a: A)(implicit F: Proc[F]): LocalT[F, C, X, A] = LocalT.pure(a)

    override def run(context: C[F])(implicit F: Proc[F]): F[X, A] =
      self.run(context).handleWith(h(_).run(context))

    override def handleWith[Y, A1 >: A](j: X => LocalT[F, C, Y, A1])(implicit F: Proc[F]): LocalT[F, C, Y, A1] =
      self.handleWith(h(_).handleWith(j))
  }

  abstract class LocalSuccess[F[+_, +_], C[f[+_, +_]], A] extends LocalT[F, C, Nothing, A] { self =>
    override def foldWith[X, B](
        h: Nothing => LocalT[F, C, X, B],
        f: A => LocalT[F, C, X, B]
    )(implicit F: Proc[F]): LocalT[F, C, X, B] = new LocalT[F, C, X, B] {
      def run(context: C[F])(implicit F: Proc[F]): F[X, B] = self.run(context).flatMap(f(_).run(context))
    }
  }

  abstract class Successful[F[+_, +_], C[f[+_, +_]], A] extends LocalSuccess[F, C, A] {
    def result(implicit F: Proc[F]): F[Nothing, A]

    def run(context: C[F])(implicit F: Proc[F]): F[Nothing, A] = result
  }

  class Pure[F[+_, +_], C[f[+_, +_]], A](val a: A) extends Successful[F, C, A] {
    def result(implicit F: Proc[F]): F[Nothing, A] = Proc.pure[F](a)
  }

  abstract class LocalFail[F[+_, +_], C[f[+_, +_]], E] extends LocalT[F, C, E, Nothing] { self =>
    override def foldWith[X, B](
        h: E => LocalT[F, C, X, B],
        f: Nothing => LocalT[F, C, X, B]
    )(implicit F: Proc[F]): LocalT[F, C, X, B] = new LocalT[F, C, X, B] {
      def run(context: C[F])(implicit F: Proc[F]): F[X, B] = self.run(context).handleWith(h(_).run(context))
    }
  }

  abstract class Failed[F[+_, +_], C[f[+_, +_]], E] extends LocalFail[F, C, E] {
    def error(implicit F: Proc[F]): F[E, Nothing]

    def run(context: C[F])(implicit F: Proc[F]): F[E, Nothing] = error
  }

  private class Raise[F[+_, +_], C[f[+_, +_]], E](val e: E) extends Failed[F, C, E] {
    def error(implicit F: Proc[F]): F[E, Nothing] = Proc.raise[F](e)
  }

}
