package manatki.akka

import akka.http.scaladsl.model.{MediaType, MediaTypes}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.{Directive, Directive0, Rejection}
import akka.http.scaladsl.server.Directives._

object http {
  //made by @odomontois requested by @aleksei_t 31.01.2018 15:20
  /** checking for Accept header verifying media type is requested by the client*/
  def withMediaType(mt: MediaType): Directive0 = Directive(continue =>
    extract(_.request.header[Accept]){
      case Some(acc) if acc.mediaRanges.exists(_.matches(MediaTypes.`application/json`)) => continue(())
      case _ => reject(UnsupportedAcceptsMediaType(mt))
    }
  )
  case class UnsupportedAcceptsMediaType(mt: MediaType) extends Rejection
}
