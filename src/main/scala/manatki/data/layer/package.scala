package manatki.data

package object layer {
  type Eval[+A] = Layer1[EvalF, A]
}
