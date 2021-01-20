package manatki.data.lambda.untyped.debrujin

import cats.Applicative
import manatki.data.tagless.{ProTraverse, ProfData, Rep}

trait Lam[-I, +O] {
  def vari(i: Int): O
  def lam(name: String, body: I): O
  def app(f: I, arg: I): O
}

object Lam extends ProfData[Lam] {
  object proTraverse extends ProTraverse[Lam] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[Rep[Lam, B]] => C) =
      new ProTraverse.Tab(left, right) with TabTraverse[F, A, B, C]
  }

  trait TabTraverse[F[_], A, B, C] extends ProTraverse.Tab[F, A, B, C, Lam] with Lam[A, C] {
    def vari(i: Int): C      = mkPure(_.vari(i))
    def lam(name: String, body: A): C      = mkMap(body)(b => _.lam(name, b))
    def app(f: A, arg: A): C = mkMap2(f, arg)((fb, argb) => _.app(fb, argb))
  }
}
