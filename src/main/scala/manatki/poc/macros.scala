package manatki.poc

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@compileTimeOnly("Annotation 'macros' is compile time only! Enable '-Ymacro-annotations' to use it.")
class macros[S, T] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TestMacros.impl
}

class TestMacros(val c: blackbox.Context) {
  import c.universe._

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] =
    c.Expr[Any](q"..$annottees")
}
