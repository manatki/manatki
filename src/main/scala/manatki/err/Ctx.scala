package manatki.err

trait Ctx {
  type Eff[E, A]
}


object Ctx {
  type Aux[+C <: Ctx, +F[e, x]] = C { type Eff[e, a] <: F[e, a] }
  type OfProc[C, F[+_]]         = C { type Eff[e <: Res, a] = ContErrT[F, C, e, a] }

  trait Of[F[_, _]] extends Ctx {
    type Eff[e, a] = F[e, a]
  }
}

