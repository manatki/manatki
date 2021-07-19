package manatki.to_fu.companions


// for the discussion in @tofu_ru started by coffius 10:34 19.07.2021 https://t.me/tofu_ru/17360
import derevo.derive
import tofu.higherKind.derived.ContextBiEmbed
import tofu.higherKind.derived.ContextEmbed
import tofu.higherKind.derived.representableB
import tofu.higherKind.derived.representableK
import tofu.optics.macros.ClassyOptics
import zio.TaskManaged
import zio.URIO
import zio.ZIO
import zio.ZManaged
import tofu.higherKind.Embed
import tofu.WithContext

trait BiComponent[Svc[+f[_, _]]] extends ContextBiEmbed[Svc] {
  type Unary[+F[_]] = Svc[Lambda[(e, a) => F[a]]]
}

trait Component[Svc[+f[_]]] extends ContextEmbed[Svc] {
  type Safe[+F[_, _]] = Svc[F[Nothing, *]]
}

@derive(representableB)
trait Foo[+F[_, _]]

object Foo extends BiComponent[Foo] {
  def impl[F[+_, +_]: Bar.Safe]: Foo[F] = new Foo[F] {}
}

@derive(representableK)
trait Bar[+F[_]]

object Bar extends Component[Bar] {
  def impl[F[+_]: Foo.Unary]: Bar[F] = new Bar[F] {}
}

@ClassyOptics
case class Context(
    foo: Foo[ZIO[Context, *, *]],
    bar: Bar[URIO[Context, *]]
)


object Context extends WithContext.Companion[Context]{
  def init: TaskManaged[Context] =
    ZManaged.succeed(
      Context(
        foo = Foo.impl[ZIO[Context, +*, +*]],
        bar = Bar.impl[URIO[Context, *]]
      )
    )
}
