package manatki.data.lambda.untyped

import manatki.data.tagless.Layer

abstract class LamNorm[-I, +O] {
  def varApp(name: String, args: Vector[I]): O
  def lam(param: String, body: I): O
}

object LamNorm{
  type T = Layer[LamNorm]

}

abstract class Lam[-I, +O] {
  def vari(name: String): O
  def app(f: I, arg: I): O
}

object Lam {
  type T = Layer[Lam]
}
