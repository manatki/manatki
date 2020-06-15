package manatki.free

import scala.reflect.ClassTag

trait Petal[-L[+f[+_, +_], +e, +a]]

object Bud {
  trait Tag extends Any

  def apply[P <: Petal[Nothing], E, A, L[+f[+_, +_], +e, +a]](
      const: L[Bud[P, +*, +*], E, A]
  ): Bud[P with Petal[L], E, A] =
    const.asInstanceOf[Bud[P with Petal[L], E, A]]

  type Base = Any { type BudOpaque }
  type T[-P <: Petal[Nothing], +E, +A] <: Base with Tag

  implicit class SingleOps[L[+f[+_, +_], +e, +a], E, A](private val bud: Bud[Petal[L], E, A]) extends AnyVal {
    def value: L[Bud[Petal[L], +*, +*], E, A] = bud.asInstanceOf[L[Bud[Petal[L], +*, +*], E, A]]
  }

  implicit class Ops[P <: Petal[Nothing], E, A](private val bud: Bud[P, E, A]) extends AnyVal {
    @inline private def distinct0[X: ClassTag, R]: Either[X, R] =
      bud match {
        case l: X            => Left(l)
        case r: R @unchecked => Right(r)
      }

    def distinct[L[+f[+_, +_], +e, +a], R <: Petal[Nothing]](implicit
        ct: ClassTag[L[Bud[P, +*, +*], E, A]],
        cover: (R with Petal[L]) <:< P
    ): Either[L[Bud[P, +*, +*], E, A], R] = distinct0[L[Bud[P, +*, +*], E, A], R]

    @inline private def match0[X: ClassTag]: Option[X] =
      bud match {
        case x: X => Some(x)
        case _    => None
      }

    def match_[L[+f[+_, +_], +e, +a]](implicit
        ct: ClassTag[L[Bud[P, +*, +*], E, A]],
    ): Option[L[Bud[P, +*, +*], E, A]] = match0[L[Bud[P, +*, +*], E, A]]
  }

}
