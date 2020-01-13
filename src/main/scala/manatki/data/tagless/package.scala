package manatki.data

package object tagless {
  type PTrans[+P[-_, +_], +F[_]] = PTrans.T[P, F]
}
