package manatki.data
import manatki.poc.macros

object Main extends App {

  @macros[
    String,
    Double,
    Int,
  ]
  class Test // compiles

}
