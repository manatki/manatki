// package manatki.to_fu.companions

// // for the discussion in @tofu_ru started by coffius 10:34 19.07.2021 https://t.me/tofu_ru/17360
// import derevo.derive
// import tofu.higherKind.derived.ContextBiEmbed
// import tofu.higherKind.derived.ContextEmbed
// import tofu.higherKind.derived.representableB
// import tofu.higherKind.derived.representableK
// import glass.macros.ClassyOptics
// import zio.TaskManaged
// import zio.URIO
// import zio.ZIO
// import zio.ZManaged
// import tofu.higherKind.Embed
// import tofu.WithContext
// import cats.Monad
// import tofu.control.Bind
// import tofu.higherKind.bi.FunctorBK
// import tofu.higherKind.bi.FunBK
// import tofu.syntax.bind._
// import tofu.bi.TwinContext
// import zio.interop.catz.monadErrorInstance
// import tofu.zioInstances.implicits._
// import glass.Contains
// import glass.Extract
// import tofu.bi.BiContext

// sealed trait Error
// object Error {
//   case class Specific1() extends Error
//   case class Specific2() extends Error
// }

// trait BiComponent[Svc[+f[_, _]]] extends ContextBiEmbed[Svc] {
//   type Unary[+F[_]] = Svc[Lambda[(e, a) => F[a]]]

//   type Eith[+F[_]] = Svc[Lambda[(e, a) => F[Either[e, a]]]]

//   implicit def eitherInstance[F[+_, +_]: Bind](implicit svc: Svc[F], Fk: FunctorBK[Svc]): Eith[F[Nothing, *]] =
//     Fk.mapb(svc)(FunBK[F](fa => fa.fold(Left(_), Right(_))))
// }

// trait Component[Svc[+f[_]]] extends ContextEmbed[Svc] {
//   type Safe[+F[_, _]] = Svc[F[Nothing, *]]
// }

// @derive(representableB)
// trait Foo[+F[_, _]] {
//   def action1: F[Error.Specific1, Unit]
//   def action2: F[Error.Specific2, Unit]
// }

// object Foo extends BiComponent[Foo] {
//   def impl[F[+_, +_]: Bar.Safe: Bind]: Foo[F] = new Foo[F] {
//     def action1: F[Error.Specific1, Unit] = Bind.raise[F](Error.Specific1())

//     def action2: F[Error.Specific2, Unit] = Bind.raise[F](Error.Specific2())

//   }
// }

// @derive(representableK)
// trait Bar[+F[_]]

// object Bar extends Component[Bar] {
//   def impl[F[+_]: Foo.Eith: Monad]: Bar[F] = new Bar[F] {}
// }

// @ClassyOptics
// case class Context(
//     foo: Foo[Context.B],
//     bar: Bar[Context.M],
// )

// object Context extends WithContext.Companion[Context] {

//   type M[+A]     = URIO[Context, A]
//   type B[+E, +A] = ZIO[Context, E, A]

//   type TContext[F[+_, +_], C] = BiContext[F, C, C]

//   final implicit def promoteBiContextStructure[A](implicit
//       field: Context Extract A,
//   ): TContext[B, A] = BiContext[B, Context, Context].extract(field, field)

//   implicitly[TContext[B, Foo[B]]]

//   implicitly[Foo.Eith[M]](
//     Foo.eitherInstance[B](implicitly, Foo.contextEmbed[B](implicitly, ???, implicitly), implicitly)
//   )
//   def init: TaskManaged[Context] =
//     ZManaged.succeed(
//       Context(
//         foo = Foo.impl[B](Bar.contextEmbed[M], implicitly),
//         bar = ???, // Bar.impl[M],
//       )
//     )
// }
