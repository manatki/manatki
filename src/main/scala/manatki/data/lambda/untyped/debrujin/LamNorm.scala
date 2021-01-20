package manatki.data.lambda.untyped.debrujin

import cats.Applicative
import manatki.data.tagless.{ProTraverse, ProfData}

trait LamNorm[-I, +O] {
  def app(idx: Int)(args: Vector[I]): O
  def lam(name: String, body: I): O
}

object LamNorm extends ProfData[LamNorm] {
  object proTraverse extends ProTraverse[LamNorm] {
    def tabTraverse[F[_]: Applicative, A, B, C](left: A => F[B])(right: F[PR[B]] => C): LamNorm[A, C] =
      new ProTraverse.Tab(left, right) with TabTrav[F, A, B, C]
  }

  trait TabTrav[F[_], A, B, C] extends ProTraverse.Tab[F, A, B, C, LamNorm] with LamNorm[A, C] {
    def app(idx: Int)(args: Vector[A]): C = mkTraverse(args)(as => _.app(idx)(as))
    def lam(name: String, body: A): C     = mkMap(body)(b => _.lam(name, b))
  }
}
