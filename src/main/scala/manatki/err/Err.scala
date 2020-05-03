package manatki.err

import cats.evidence.As
import manatki.err.Attempt.Attempt

import scala.annotation.unchecked.uncheckedVariance

trait Err {
  type Result
}

trait OnSuccess[-A] extends Err {
  def success(a: A): Result
}

object Err {
  private object err extends Err
  type Aux[+E <: Err, A] = E { type Result = A }
  def success[A]: Aux[Err, A] = err.asInstanceOf[Aux[Err, A]]
}

trait Opt extends Err {
  def none: Result
}

trait Raisen[-X <: Err] extends Throwable {
  def handle(x: X): x.Result

  def attempt: Attempt[X, Nothing] = this.asInstanceOf[Attempt[X, Nothing]]
}

object Raisen {
  def apply[E <: Err]: Applied[E] = new Applied[E](true)

  class Applied[E <: Err](private val __ : Boolean) extends AnyVal {
    type Arb
    def apply(l: Lam[E, Arb]): Raisen[E] = l
  }

  abstract class Lam[E <: Err, R] extends Raisen[E]{
    def lamApply(e: Err.Aux[E, R]): R

    def handle(x: E): x.Result = lamApply(x.asInstanceOf[Err.Aux[E, R]]).asInstanceOf[x.Result]
  }

}

object Attempt {
  trait Tag extends Any
  type Attempt[-E <: Err, +A]

  def success[A](x: A): Attempt[Err, A]             = x.asInstanceOf[Attempt[Err, A]]
  def asSuccess[A]: A As Attempt[Err, A]            = As.refl[A].asInstanceOf[A As Attempt[Err, A]]
  def subst[F[+_], A](fa: F[A]): F[Attempt[Err, A]] = fa.asInstanceOf[F[Attempt[Err, A]]]

  implicit class Ops[A, E <: Err](private val attempt: Attempt[E, A]) extends AnyVal {
    def fold(fail: E)(success: A => fail.Result): fail.Result = attempt match {
      case r: Raisen[E] @unchecked => r.handle(fail)
      case a: A @unchecked         => success(a)
    }

    def map[B](f: A => B): Attempt[E, B] = attempt match {
      case r: Raisen[E] @unchecked => r.attempt
      case a: A @unchecked         => success(f(a))
    }

    def atmap[B](f: A => B): Attempt[E, B] = map(f)

    def recov[A1 >: A](e: Err.Aux[E, A1]): A1 = attempt match {
      case r: Raisen[E] @unchecked => r.handle(e)
      case a: A @unchecked         => a
    }
  }
}
