package js7.common.http

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.util.ByteString
import io.circe.{Decoder, Encoder, Json, Printer, jawn}
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.CirceUtils.implicits.CompactPrinter
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils.syntax._

object CirceJsonSupport
{
  implicit final def jsonMarshaller[A: Encoder](implicit printer: Printer = CompactPrinter): ToEntityMarshaller[A] =
    jsonJsonMarshaller(printer) compose implicitly[Encoder[A]].apply

  implicit final def jsonJsonMarshaller(implicit printer: Printer = CompactPrinter): ToEntityMarshaller[Json] =
    Marshaller.withFixedContentType(`application/json`) { json =>
      HttpEntity(`application/json`, printer.print(json))
    }

  implicit final def unmarshaller[A: Decoder]: FromEntityUnmarshaller[A] =
    jsonUnmarshaller.map(json =>
      implicitly[Decoder[A]]
        .decodeJson(json)
        .toChecked/*renders message*/
        .orThrowWithoutStacktrace)

  implicit final val jsonUnmarshaller: FromEntityUnmarshaller[Json] =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .map {
        case ByteString.empty => throw Unmarshaller.NoContentException
        case byteString => jawn.parseByteBuffer(byteString.asByteBuffer).toChecked.orThrow
      }
}
