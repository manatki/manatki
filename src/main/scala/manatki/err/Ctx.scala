package manatki.err

trait Ctx {
  type Eff[E, _]
}

object Ctx {
  type Aux[+C <: Ctx, +F[e, x]] = C { type Eff[e <: Err, a] <: F[e, a] }
  type OfProc[C <: Ctx, F[+_]]  = C { type Eff[e <: Err, a] = Proc[F, C, e, a] }

  trait Of[F[_, _]] extends Ctx {
    type Eff[e, a] = F[e, a]
  }
}
