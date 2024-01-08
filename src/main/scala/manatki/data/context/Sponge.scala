package manatki.data.context

import glass.Contains
import glass.Equivalent
import glass.macros.GenEquivalent

case class Sponge[F[C, _], Ctx[f[_]]](wrapped: Ctx[F[Sponge[F, Ctx], *]])

object Sponge {
  implicit def iso[F[C, _], Ctx[f[_]]]: Equivalent[Sponge[F, Ctx], Ctx[F[Sponge[F, Ctx], *]]] = GenEquivalent.apply

  implicit def lens[F[C, _], Ctx[f[_]], A](implicit
      l: Contains[Ctx[F[Sponge[F, Ctx], *]], A]
  ): Contains[Sponge[F, Ctx], A] = iso >> l
}
