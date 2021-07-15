package manatki.higherKinded

import cats.{Id, ~>, Applicative}
import cats.arrow.{FunctionK}
import tofu.higherKind.bi.FunBK

// simple mixin with usable type aliases
trait Fiction[Q[_]] {
  type A[arg]      = Q[Fiction.Arg[arg]]
  type S[err, res] = Q[Fiction.Sub[err, res]]
  type R[err, res] = Q[Fiction.Ret[err, res]]
}

object Fiction {
  // tags for type argument combination
  final class Arg[+A]
  final class Sub[+E, +A]
  final class Ret[+E, +A]
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
  def continue[R[e, a]](f: fiction[Exposition[P, S, R, *]]): R[E, A]
}

// natural transformation from representation to target R
trait Retell[fiction[q[_]], P[_], S[_, _], R[_, _], F[_], G[_]] {
  def apply[E, A](src: F[G[Fabula[fiction, P, S, E, A]]]): R[E, A]
}

// base type for hk trifunctor typeclasses with handy alias
trait FicBase[fiction[q[_]]] {
  type Exposed[P[_], S[_, _], R[_, _]] = fiction[Exposition[P, S, R, *]]
}

// contravariant hk functor on arguments
trait MapArg[fiction[q[_]]] extends FicBase[fiction] {
  def mapArg[LP[_], RP[_], S[_, _], R[_, _]](
      f: LP ~> RP,
      fiction: Exposed[RP, S, R],
  ): Exposed[LP, S, R]
}

// contravariant hk functor on subprocesses
trait MapSub[fiction[q[_]]] extends FicBase[fiction] {
  def mapSub[P[_], LS[_, _], RS[_, _], R[_, _]](
      f: LS FunBK RS,
      fiction: Exposed[P, RS, R],
  ): Exposed[P, LS, R]
}

// covariant hk functor on returns
trait MapRet[fiction[q[_]]] extends FicBase[fiction] {
  def mapRet[P[_], S[_, _], LR[_, _], RR[_, _]](
      f: LR FunBK RR,
      fiction: Exposed[P, S, LR],
  ): Exposed[P, S, RR]
}

// representable instance of trifunctor (or biprofunctor idk)
trait Plot[fiction[q[_]]] extends MapRet[fiction] {
  def plot[P[_], S[_, _], R[_, _]](retell: Retell[fiction, P, S, R, Id, Id]): Exposed[P, S, R]

  def mapRet[P[_], S[_, _], LR[_, _], RR[_, _]](f: FunBK[LR, RR], fiction: Exposed[P, S, LR]): Exposed[P, S, RR] =
    plot(new Retell[fiction, P, S, RR, Id, Id] {
      def apply[E, A](src: Fabula[fiction, P, S, E, A]): RR[E, A] = f(src.continue(fiction))
    })
}

//representable , also requiring bitraversing
trait Narrative[fiction[q[_]]] extends Plot[fiction] with MapArg[fiction] with MapSub[fiction] {
  import Narrative._
  def narrative[IP[_], IS[_, _], P[_], S[_, _], R[_, _], PF[_]: Applicative, SF[_]: Applicative](
      argScene: Scene1[IP, PF, P],
      subScene: Scene2[IS, SF, S],
      augRetell: Retell[fiction, P, S, R, PF, SF]
  ): Exposed[IP, IS, R]

  override def plot[P[_], S[_, _], R[_, _]](retell: Retell[fiction, P, S, R, Id, Id]): fiction[Exposition[P, S, R, *]] =
    narrative[P, S, P, S, R, Id, Id](FunctionK.id[P], FunBK[S](x => x), retell)

  def bicontramap[IP[_], IS[_, _], P[_], S[_, _], R[_, _]](
      argF: IP ~> P,
      subF: IS FunBK S,
      fiction: Exposed[P, S, R]
  ): Exposed[IP, IS, R] =
    narrative[IP, IS, P, S, R, Id, Id](
      argF,
      subF,
      new Retell[fiction, P, S, R, Id, Id] {
        def apply[E, A](src: Fabula[fiction, P, S, E, A]): R[E, A] = src.continue(fiction)
      }
    )

  override def mapArg[LP[_], RP[_], S[_, _], R[_, _]](
      f: FunctionK[LP, RP],
      fiction: Exposed[RP, S, R]
  ): Exposed[LP, S, R] = bicontramap[LP, S, RP, S, R](f, FunBK[S](x => x), fiction)

  override def mapSub[P[_], LS[_, _], RS[_, _], R[_, _]](
      f: FunBK[LS, RS],
      fiction: Exposed[P, RS, R]
  ): Exposed[P, LS, R] = bicontramap[P, LS, P, RS, R](FunctionK.id, f, fiction)

}

object Narrative {
  type Scene1[F[_], G[_], H[_]]       = FunctionK[F, Lambda[a => G[H[a]]]]
  type Scene2[F[_, _], G[_], H[_, _]] = FunBK[F, Lambda[(a, b) => G[H[a, b]]]]
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
  import Narrative._
  implicit val narrative: Narrative[Authorization] =
    new Narrative[Authorization] {
      def narrative[IP[_], IS[_, _], P[_], S1[_, _], R1[_, _], PF[_]: Applicative, SF[_]: Applicative](
          argScene: Scene1[IP, PF, P],
          subScene: Scene2[IS, SF, S1],
          augRetell: Retell[Authorization, P, S1, R1, PF, SF]
      ): Exposed[IP, IS, R1] = new Exposed[IP, IS, R1] {
        import Exposition._
        def authorize[E, T](ftoken: A[Token], faction: S[E, T]): R[Either[E, AuthError], T] =
          Ret(
            augRetell(
              argScene(ftoken.arg[Token]).map { pt =>
                subScene(faction.sub).map { fa =>
                  new Fabula[Authorization, P, S1, Either[E, AuthError], T] {
                    def continue[R[e, a]](auth: Exposed[P, S1, R]): R[Either[E, AuthError], T] =
                      auth.authorize(Arg(pt), Sub(fa)).ret
                  }
                }
              }
            )
          )
        def authenticate(fusername: A[String], fpass: A[String]): R[AuthError, Token]       =
          Ret(
            augRetell(
              argScene(fusername.arg).map2(argScene(fpass.arg)) { (username, pass) =>
                Applicative[SF].pure(new Fabula[Authorization, P, S1, AuthError, Token] {
                  def continue[R[e, a]](auth: Exposed[P, S1, R]): R[AuthError, Token] =
                    auth.authenticate(Arg(username), Arg(pass)).ret
                })
              }
            )
          )
      }
    }
}
