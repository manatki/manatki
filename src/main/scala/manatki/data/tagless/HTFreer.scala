package manatki.data.tagless

import cats.free.Free
import manatki.data.tagless.HTFreer.{HTBind, HTLiftBind, HTLiftLift, HTLiftPure, HTPure}
import tofu.optics.data.Identity

import scala.annotation.tailrec

sealed trait HTFreer[-U[-_[+_], +_[+_]], +A] {
  def flatMap[U1[-i[+_], +o[+_]] <: U[i, o], B](f: A => HTFreer[U1, B]): HTFreer[U1, B]
}

object HTFreer {
  final case class HTPure[+A](a: A) extends HTFreer[Any, A] {
    def flatMap[U1[-_[+_], +_[+_]], B](f: A => HTFreer[U1, B]): HTFreer[U1, B] = f(a)
  }

  abstract class HTBind[-U[-_[+_], +_[+_]], F[+_], A, +B] extends HTFreer[U, B] { self =>
    def head[T[+_]](handle: U[F, T]): T[A]
    def body[X](ix: F[X]): HTFreer[U, X]
    def tail(a: A): HTFreer[U, B]

    def flatMap[U1[-f[+_], +g[+_]] <: U[f, g], C](f: B => HTFreer[U1, C]): HTBind[U1, F, A, C] =
      new HTBind[U1, F, A, C] {
        def tail(a: A): HTFreer[U1, C]          = self.tail(a).flatMap(f)
        def body[X](fx: F[X]): HTFreer[U1, X]   = self.body(fx)
        def head[T[+_]](handle: U1[F, T]): T[A] = self.head[T](handle)

        override def flatMap[U2[-f[+_], +g[+_]] <: U1[f, g], D](g: C => HTFreer[U2, D]): HTBind[U2, F, A, D] =
          self.flatMap(a => f(a).flatMap(g))
      }
  }

  abstract class HTLiftBind[U[-_[+_], +_[+_]], A, +B] extends HTBind[U, Identity, A, B] {
    def body[X](x: X): HTFreer[Any, X] = HTPure(x)
  }

  abstract class HTLiftPure[U[-_[+_], +_[+_]], A] extends HTLiftBind[U, A, A] {
    def tail(a: A): HTFreer[Any, A] = HTPure(a)
  }

  abstract class HTLiftLift[U[-_[+_], +_[+_]], A] extends HTLiftBind[U, HTFreer[U, A], A] {
    def tail(fa: HTFreer[U, A]): HTFreer[U, A] = fa
  }

  implicit class HTFreerPureOps[A](private val ht: HTFreer[Any, A]) extends AnyVal {
    @tailrec def value: A = ht match {
      case HTPure(a)                 => a
      case hbind: HTBind[u, f, a, A] => hbind.tail(hbind.head[Identity](())).value
    }
  }
}

trait HFTrans[+U[-_[+_], +_[+_]], -V[-_[+_], +_[+_]]] {
  type VF[+A] <: HTFreer[V, A]
  def apply[F[+_], A]: U[VF, VF]
}

sealed trait ReaderFX[R, -I[+_], +O[+_]] {
  def ask: O[R]
  def local[A](fa: I[A], f: R => R): O[A]
}

object ReaderFX {
  type Fru[R, U[-i[+_], +o[+_]], +A] = HTFreer[λ[(`-i[+_]`, `+o[+_]`) => ReaderFX[R, i, o] with U[i, o]], A]
  def ops[U[-i[+_], +o[+_]], R]: ReaderFX[R, HTFreer[U, +*], Fru[R, U, +*]] =
    new ReaderFX[R, HTFreer[U, +*], Fru[R, U, +*]] {
      type UT[-i[+_], +o[+_]] = ReaderFX[R, i, o] with U[i, o]
      def ask: Fru[R, U, R] = new HTLiftPure[UT, R] {
        def head[T[+_]](handle: UT[Identity, T]): T[R] = handle.ask
      }

      def local[A](fa: HTFreer[U, A], f: R => R): Fru[R, U, A] = new HTLiftLift[UT, A] {
        def head[T[+_]](handle: UT[Identity, T]): T[HTFreer[U, A]] = handle.local(fa, f)
      }
    }
}
