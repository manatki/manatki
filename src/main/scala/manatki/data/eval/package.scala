package manatki.data
import cats.Functor

package object eval {
  type NothingT[+A] = Nothing


  implicit val nothingFunctor: Functor[Nothing]  = new Functor[Nothing] {
    def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }
}
