package manatki.data.lambda.untyped

abstract class Lam[-I, +O] {
  def vari(name: String): O
  def lam(param: String, body: I): O
  def app(f: I, arg: I): O
}

object Lam{

}
