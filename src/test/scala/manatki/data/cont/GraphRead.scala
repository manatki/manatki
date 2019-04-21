package manatki.data.cont

import cats.mtl.ApplicativeLocal
import cats.tagless.Derive
import cats.{Applicative, Monad}
import manatki.data.cont.GraphRead.Value

trait GraphRead[A] {
  def from(acc: Value, name: List[Char]): A
  def to(acc: Value, from: String, to: List[String], name: List[Char]): A
}

object GraphRead {
  type Value = List[(String, List[String])]

  implicit val local = new Monad[GraphRead] with ApplicativeLocal[GraphRead, Val[GraphRead]] {
    private val derived = Derive.flatMap[GraphRead]

    def local[A](f: Val[GraphRead] => Val[GraphRead])(fa: GraphRead[A]): GraphRead[A] =
      new GraphRead[A] {
        def from(acc: Value, name: List[Char]): A =
          f(new Val[GraphRead] { def cont[B](k: GraphRead[B]): B = k.from(acc, name) }).cont(fa)
        def to(acc: Value, from: String, to: List[String], name: List[Char]): A =
          f(new Val[GraphRead] { def cont[B](k: GraphRead[B]): B = k.to(acc, from, to, name) }).cont(fa)
      }

    def scope[A](e: Val[GraphRead])(fa: GraphRead[A]): GraphRead[A] = local(identity)(fa)

    val applicative: Applicative[GraphRead] = this

    def ask: GraphRead[Val[GraphRead]] = new GraphRead[Val[GraphRead]] {
      def from(acc: Value, name: List[Char]) = new Val[GraphRead] {
        def cont[B](k: GraphRead[B]): B = k.from(acc, name)
      }
      def to(acc: Value, from: String, to: List[String], name: List[Char]) = new Val[GraphRead] {
        def cont[B](k: GraphRead[B]): B = k.to(acc, from, to, name)
      }
    }

    def reader[A](f: Val[GraphRead] => A): GraphRead[A] = map(ask)(f)

    def pure[A](x: A): GraphRead[A] = new GraphRead[A] {
      def from(acc: Value, name: List[Char]): A                               = x
      def to(acc: Value, from: String, to: List[String], name: List[Char]): A = x
    }

    def flatMap[A, B](fa: GraphRead[A])(f: A => GraphRead[B]): GraphRead[B] = derived.flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => GraphRead[Either[A, B]]): GraphRead[B] = derived.tailRecM(a)(f)
  }

}

trait Val[F[_]] {
  def cont[B](k: F[B]): B
}

object Val {
  type Mod[F[_]] = F[Val[F]]

  def mod[F[_]](m: Mod[F])(v: Val[F]): Val[F] = v.cont(m)
}

trait MonadLocalT[F[_]] extends Monad[F] {
  def local[A](gr: Val.Mod[F], fa: F[A]): F[A]
}
