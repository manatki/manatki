package manatki.data
import cats.Id

package object tagless {
  type PTrans[+P[-_, +_], +F[_]] = PTrans.T[P, F]
  type PDistr[P[-_, +_], F[_]]   = PDistr.T[P, F]

  type Builder[-P[-_, +_], A] = GBuilder[P, Id, A]
}
