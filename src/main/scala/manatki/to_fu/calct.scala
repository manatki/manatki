package manatki.to_fu

import cats.Monad
import tofu.syntax.monadic._
import tofu.data.CalcT
import tofu.data.calc.StepResult._

// requested in @tofu_ru by @sigevsky 16.07.2021 12:12
object calct {
  // run endofunctorial CalcT using monadic tailrec
  def tailrecEither[F[+_]: Monad, R, S1, S2, E, A](
      calc: CalcT[F, R, S1, S2, E, A]
  )(r: R, init: S1): F[(S2, Either[E, A])] =
    calc.step(r, init).tailRecM {
      case Ok(state, value)                   => Right((state, Right(value))).pure[F]
      case Error(state, err)                  => Right((state, Left(err))).pure[F]
      case wrap @ Wrap(input, state, _, cont) =>
        wrap.inner.map(x => Left(cont.success(state, x).step(input, wrap.state)))
    }
}
