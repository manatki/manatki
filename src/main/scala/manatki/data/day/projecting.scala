package manatki.data.day

import cats.arrow.FunctionK
import cats.{Comonad, Functor, ~>}
import manatki.syntax.functionK

object projecting extends DayColifting {
  implicit def projectId[F[_]]: F ~> F = FunctionK.id
}

trait DayColifting {
  final implicit def recProjectLeft[F[_]: Functor, G[_]: Comonad, H[_]](implicit rec: F ~> H): Day[F, G, ?] ~> H =
    functionK[Day[F, G, ?]](_.projectL) andThen rec

  final implicit def recProjectRight[F[_]: Comonad, G[_]: Functor, H[_]](implicit rec: G ~> H): Day[F, G, ?] ~> H =
    functionK[Day[F, G, ?]](_.projectR) andThen rec
}
