package manatki.fsfs
import org.http4s.HttpRoutes
import tofu.lift.Unlift
import cats.data.Kleisli
import cats.Monad
import tofu.syntax.monadic._
import cats.data.OptionT
import tofu.lift.IsoK
import cats.Functor
import tofu.higherKind.Embed
import org.http4s._

object httpfors {  
  def imapRoutes[F[_], G[_]: Functor](routes: HttpRoutes[F])(implicit FG: IsoK[F, G]): HttpRoutes[G] = 
    Kleisli(greq => routes.run(greq.mapK(FG.fromF)).mapK(FG.tof).map(_.mapK(FG.tof)))

  // requested bu LolDog in PONV 15.06.2020 https://t.me/scala_ponv/972282
  def unliftRoutes[F[_], G[_]: Monad](routes: HttpRoutes[F])(implicit FG: Unlift[F, G]): HttpRoutes[G] = 
    Embed.of[Http[*[_], G], OptionT[G, *]](OptionT.liftF(FG.subIso.map(implicit iso => imapRoutes[F, G](routes))))
}
