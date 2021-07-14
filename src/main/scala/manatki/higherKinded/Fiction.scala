package manatki.higherKinded

import cats.Applicative
import manatki.higherKinded
import cats.Id

// simple mixin with usable type aliases
trait Fiction[Q[_]] {
  type A[arg]      = Q[Fiction.Arg[arg]]
  type S[err, res] = Q[Fiction.Sub[err, res]]
  type R[err, res] = Q[Fiction.Ret[err, res]]
}

object Fiction {
  ???
  // tags for type argument combination
  sealed trait Place
  final class Arg[+A]     extends Place
  final class Sub[+E, +A] extends Place
  final class Ret[+E, +A] extends Place
}

// closed family encoded as GADT
sealed trait Exposition[P[_], S[_, _], R[_, _], T] {
  import Exposition._
  type Z[x] = Exposition[P, S, R, x]
  def arg[A](implicit ev: T =:= Fiction.Arg[A]): P[A] =
    ev.liftCo[Z](this) match {
      case Arg(arg) => arg
    }

  def sub[E, A](implicit ev: T =:= Fiction.Sub[E, A]): S[E, A] =
    ev.liftCo[Z](this) match {
      case Sub(sub) => sub
    }

  def ret[E, A](implicit ev: T =:= Fiction.Ret[E, A]): R[E, A] =
    ev.liftCo[Z](this) match {
      case Ret(ret) => ret
    }
}
object Exposition                                  {
  final case class Arg[P[_], S[_, _], R[_, _], A](arg: P[A])       extends Exposition[P, S, R, Fiction.Arg[A]]
  final case class Sub[P[_], S[_, _], R[_, _], E, A](sub: S[E, A]) extends Exposition[P, S, R, Fiction.Sub[E, A]]
  final case class Ret[P[_], S[_, _], R[_, _], E, A](ret: R[E, A]) extends Exposition[P, S, R, Fiction.Ret[E, A]]

  type Augment[P[_], S[_, _], R[_, _], PF[_], SF[_], A] =
    Exposition[Lambda[a => PF[P[a]]], Lambda[(e, a) => SF[S[e, a]]], R, A]
}

//representation of trait modulo R
trait Fabula[fiction[q[_]], P[_], S[_, _], E, A] {
  def continue[R[+e, +a]](f: fiction[Exposition[P, S, R, *]]): R[E, A]
}

// natural transformation from representation to target R
trait Retell[fiction[q[_]], P[_], S[_, _], R[_, _], F[_], G[_]] {
  def apply[E, A](src: F[G[Fabula[fiction, P, S, E, A]]]): R[E, A]
}

// representable instance of trifunctor (or biprofunctor idk)
trait Plot[fiction[q[_]]] {
  def plot[P[_], S[_, _], R[_, _]](retell: Retell[fiction, P, S, R, Id, Id]): fiction[Exposition[P, S, R, *]]
}

//representable , also requiring bitraversing
trait Narrative[fiction[q[_]]] extends Plot[fiction] {
  def narrative[P[_], S[_, _], R[_, _], PF[_]: Applicative, SF[_]: Applicative](
      augRetell: Retell[fiction, P, S, R, PF, SF]
  ): fiction[Exposition.Augment[P, S, R, PF, SF, *]]

  override def plot[P[_], S[_, _], R[_, _]](retell: Retell[fiction, P, S, R, Id, Id]): fiction[Exposition[P, S, R, *]] =
    narrative[P, S, R, Id, Id](retell)
}

///
/// EXAMPLE
///
case class AuthError(message: String)
case class Token(token: String)
trait Authorization[Q[_]] extends Fiction[Q] {
  def authorize[E, T](token: A[Token], action: S[E, T]): R[Either[E, AuthError], T]
  def authenticate(username: A[String], pass: A[String]): R[AuthError, Token]
}

import tofu.syntax.monadic._
object Authorization {
  implicit val narrative: Narrative[Authorization] =
    new Narrative[Authorization] {
      def narrative[P[_], S1[_, _], R1[_, _], PF[_]: Applicative, SF[_]: Applicative](
          augRetell: Retell[Authorization, P, S1, R1, PF, SF]
      ): Authorization[Exposition.Augment[P, S1, R1, PF, SF, *]] =
        new Authorization[Exposition.Augment[P, S1, R1, PF, SF, *]] {
          import Exposition._
          def authorize[E, T](ftoken: A[Token], faction: S[E, T]): R[Either[E, AuthError], T] =
            Ret(
              augRetell(
                ftoken.arg[Token].map { pt =>
                  faction.sub.map { fa =>
                    new Fabula[Authorization, P, S1, Either[E, AuthError], T] {
                      def continue[R[+e, +a]](
                          auth: Authorization[Exposition[P, S1, R, *]]
                      ): R[Either[E, AuthError], T] =
                        auth.authorize(Arg(pt), Sub(fa)).ret
                    }
                  }
                }
              )
            )
          def authenticate(fusername: A[String], fpass: A[String]): R[AuthError, Token]       =
            Ret(
              augRetell(
                fusername.arg.map2(fpass.arg) { (username, pass) =>
                  Applicative[SF].pure(new Fabula[Authorization, P, S1, AuthError, Token] {
                    def continue[R[+e, +a]](auth: Authorization[Exposition[P, S1, R, *]]): R[AuthError, Token] =
                      auth.authenticate(Arg(username), Arg(pass)).ret
                  })
                }
              )
            )

        }
    }
}
