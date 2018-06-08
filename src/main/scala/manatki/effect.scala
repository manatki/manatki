package manatki

import cats.effect.{Effect, IO}

object effect {
  implicit class EffectOps[F[_], A](val fa: F[A]) extends AnyVal {
    // mentioned by @notxcain 17.01.2018 12:52
    // provided by built-in since cats-effect 1.0.0-RC2
    def toIO(implicit Eff: Effect[F]): IO[A] =
      IO.async(cb => Eff.runAsync(fa)(x => IO(cb(x))).unsafeRunAsync(_ => ()))
  }
}
