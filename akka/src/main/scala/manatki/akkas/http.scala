package manatki.akkas

import akka.http.scaladsl.model.{MediaType, MediaTypes}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.{Directive, Directive0, Rejection}
import akka.http.scaladsl.server.Directives._
import io.circe.Codec
import akka.http.scaladsl.model.HttpHeader
import io.circe.Json
import io.circe.{Decoder, HCursor}
import io.circe.syntax._
import HttpHeader.ParsingResult
import io.circe.ParsingFailure
import io.circe.DecodingFailure

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



  //by request of @imgestalt 2.06.2020 8:21
  implicit def headerCodec: Codec.AsArray[HttpHeader] = new Codec.AsArray[HttpHeader]{
    def encodeArray(a: HttpHeader): Vector[Json] = Vector(a.name().asJson, a.value().asJson)

    private def parseHeader(name: String, value: String): Decoder.Result[HttpHeader] = 
      HttpHeader.parse(name, value) match {
        case ParsingResult.Error(error) => Left(DecodingFailure(error.formatPretty, Nil))
        case ParsingResult.Ok(header, errors) => Right(header)
      }

    def apply(c: HCursor): Decoder.Result[HttpHeader] = {
      val lst = c.downArray
      for {
        name <- lst.as[String]
        value <- lst.right.as[String]
        res <- parseHeader(name, value)
      } yield res
    }
  }
}
