package manatki

package object err {
  type Res = {
    type Result
  }

  type CtxLoc[C, F[+_, +_]] = C {type Eff[e, a] = LocalT[F, C, e, a]}
}
