package manatki.err

import manatki.err.proc._

trait LocalT[+F[+_, +_], C[f[_, _]], +E, +A] { self =>
  def run[F1[+e, +a] >: F[e, a]: Proc](context: C[F1]): F1[E, A]

  def foldWith[F1[+e, +a] >: F[e, a], X, B](
      h: E => LocalT[F1, C, X, B],
      f: A => LocalT[F1, C, X, B]
  ): LocalT[F1, C, X, B] = new LocalT.FoldWith(self, h, f)

  def flatMap[F1[+e, +a] >: F[e, a]: Proc, E1 >: E, B](f: A => LocalT[F1, C, E1, B]): LocalT[F1, C, E1, B] =
    new LocalT.FlatMap(self, f)

  def handleWith[F1[+e, +a] >: F[e, a]: Proc, X, A1 >: A](f: E => LocalT[F1, C, X, A1]): LocalT[F1, C, X, A1] =
    new LocalT.HandleWith(self, f)

}

object LocalT {
  def pure[F[+_, +_], C[f[_, _]]] = new PureApp[F, C](true)

  class PureApp[F[+_, +_], C[f[_, _]]](private val __ : Boolean) extends AnyVal {
    def apply[A](a: A)(implicit F: Proc[F]): LocalT[F, C, Nothing, A] = new Pure(a)
  }

  def raise[F[+_, +_], C[f[_, _]]] = new RaiseApp[F, C](true)

  class RaiseApp[F[+_, +_], C[f[_, _]]](private val __ : Boolean) extends AnyVal {
    def apply[E](e: E)(implicit F: Proc[F]): LocalT[F, C, E, Nothing] = new Raise(e)
  }

  private abstract class FoldWithA[+F[+_, +_], C[f[_, _]], E, A, +X, +B](
      self: LocalT[F, C, E, A],
  ) extends LocalT[F, C, X, B] { up =>

    def handler(e: E): LocalT[F, C, X, B]
    def successor(a: A): LocalT[F, C, X, B]
    def run[F1[+e, +a] >: F[e, a]: Proc](context: C[F1]): F1[X, B] =
      self.run(context).foldWith(handler(_).run(context), successor(_).run(context))

    override def foldWith[F1[+e, +a] >: F[e, a], U, D](
        j: X => LocalT[F1, C, U, D],
        g: B => LocalT[F1, C, U, D]
    ): LocalT[F1, C, U, D] =
      new FoldWithA[F1, C, E, A, U, D](self) {
        def handler(e: E): LocalT[F1, C, U, D]   = up.handler(e).foldWith(j, g)
        def successor(a: A): LocalT[F1, C, U, D] = up.successor(a).foldWith(j, g)
      }

    override def flatMap[F1[+e, +a] >: F[e, a]: Proc, E1 >: X, D](f: B => LocalT[F1, C, E1, D]): LocalT[F1, C, E1, D] =
      new FoldWithA[F1, C, E, A, E1, D](self) {
        def handler(e: E): LocalT[F1, C, E1, D]   = up.handler(e).flatMap(f)
        def successor(a: A): LocalT[F1, C, E1, D] = up.successor(a).flatMap(f)
      }

    override def handleWith[F1[+e, +a] >: F[e, a]: Proc, Y, B1 >: B](
        h: X => LocalT[F1, C, Y, B1]
    ): LocalT[F1, C, Y, B1] =
      new FoldWithA[F1, C, E, A, Y, B1](self) {
        def handler(e: E): LocalT[F1, C, Y, B1]   = up.handler(e).handleWith(h)
        def successor(a: A): LocalT[F1, C, Y, B1] = up.successor(a).handleWith(h)
      }
  }

  private class FoldWith[+F[+_, +_], C[f[_, _]], E, A, +X, +B](
      self: LocalT[F, C, E, A],
      h: E => LocalT[F, C, X, B],
      f: A => LocalT[F, C, X, B]
  ) extends FoldWithA[F, C, E, A, X, B](self) {
    def handler(e: E): LocalT[F, C, X, B] = h(e)

    def successor(a: A): LocalT[F, C, X, B] = f(a)
  }

  private class FlatMap[+F[+_, +_]: Proc, C[f[_, _]], E, A, +B](
      self: LocalT[F, C, E, A],
      f: A => LocalT[F, C, E, B]
  ) extends FoldWithA[F, C, E, A, E, B](self) {
    def handler(e: E): LocalT[F, C, E, B] = LocalT.raise[F, C](e)

    def successor(a: A): LocalT[F, C, E, B] = f(a)

    override def run[F1[+e, +a] >: F[e, a]: Proc](context: C[F1]): F1[E, B] =
      self.run(context).flatMap(f(_).run(context))

    override def flatMap[F1[+e, +a] >: F[e, a]: Proc, E1 >: E, D](g: B => LocalT[F1, C, E1, D]): LocalT[F1, C, E1, D] =
      self.flatMap(f(_).flatMap(g))
  }

  private class HandleWith[+F[+_, +_]: Proc, C[f[_, _]], E, A, +X](
      self: LocalT[F, C, E, A],
      h: E => LocalT[F, C, X, A]
  ) extends FoldWithA[F, C, E, A, X, A](self) {
    def handler(e: E): LocalT[F, C, X, A] = h(e)

    def successor(a: A): LocalT[F, C, X, A] = LocalT.pure(a)

    override def run[F1[+e, +a] >: F[e, a]: Proc](context: C[F1]): F1[X, A] =
      self.run(context).handleWith(h(_).run(context))

    override def handleWith[F1[+e, +a] >: F[e, a]: Proc, Y, A1 >: A](
        j: X => LocalT[F1, C, Y, A1]
    ): LocalT[F1, C, Y, A1] =
      self.handleWith(h(_).handleWith(j))
  }

  private class Successful[+F[+_, +_]: Proc, C[f[_, _]], A](val fa: F[Nothing, A]) extends LocalT[F, C, Nothing, A] {
    def run[F1[+e, +a] >: F[e, a]: Proc](context: C[F1]): F1[Nothing, A] = fa

    override def foldWith[F1[+e, +a] >: F[e, a], X, B](
        h: Nothing => LocalT[F1, C, X, B],
        f: A => LocalT[F1, C, X, B]
    ): LocalT[F1, C, X, B] = new LocalT[F1, C, X, B] {
      def run[F2[+e, +a] >: F1[e, a]: Proc](context: C[F2]): F2[X, B] = fa.flatMap(f(_).run(context))
    }
  }

  private class Pure[+F[+_, +_]: Proc, C[f[_, _]], A](val a: A) extends Successful[F, C, A](Proc.pure[F](a))

  private class Failed[+F[+_, +_], C[f[_, _]], E](val fa: F[E, Nothing]) extends LocalT[F, C, E, Nothing] {
    def run[F1[+e, +a] >: F[e, a]: Proc](context: C[F1]): F1[E, Nothing] = fa
  }

  private class Raise[+F[+_, +_]: Proc, C[f[_, _]], E](val e: E) extends Failed[F, C, E](Proc.raise[F](e))

}
