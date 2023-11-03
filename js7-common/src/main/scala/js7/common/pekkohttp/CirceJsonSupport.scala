package js7.common.pekkohttp

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, Printer, jawn}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.CirceUtils.implicits.CompactPrinter
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.utils.ScalaUtils.syntax.*
import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import org.apache.pekko.http.scaladsl.model.ContentTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import org.apache.pekko.util.ByteString
import scala.util.control.NonFatal

object CirceJsonSupport
{
  private val logger = Logger[this.type]

  implicit def jsonUnmarshaller[A: Decoder]: FromEntityUnmarshaller[A] =
    unmarshaller[A]

  implicit def jsonMarshaller[A](implicit encoder: Encoder[A], printer: Printer = CompactPrinter): ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(`application/json`) { value =>
      val string = logException(s"jsonMarhaller(${encoder.getClass.getName})") {
        printer.print(value.asJson)
      }
      HttpEntity.Strict(`application/json`, ByteString(string))
    }

  private def logException[A](what: => String)(body: => A): A =
    try body
    catch { case NonFatal(t) =>
      logger.warn(s"jsonMarhaller($what: ${t.toStringWithCauses}", t)
      throw t
    }

  //implicit final def jsonMarshaller[A: Encoder](implicit printer: Printer = CompactPrinter): ToEntityMarshaller[A] =
  //  jsonJsonMarshaller(printer) compose implicitly[Encoder[A]].apply

  implicit final def jsonJsonMarshaller(implicit printer: Printer = CompactPrinter): ToEntityMarshaller[Json] =
    Marshaller.withFixedContentType(`application/json`) { json =>
      HttpEntity(`application/json`, printer.print(json))
    }

  implicit final def unmarshaller[A: Decoder]: FromEntityUnmarshaller[A] =
    jsonJsonUnmarshaller.map(json =>
      implicitly[Decoder[A]]
        .decodeJson(json)
        .toChecked/*renders message*/
        .orThrowWithoutStacktrace)

  implicit final val jsonJsonUnmarshaller: FromEntityUnmarshaller[Json] =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .map {
        case ByteString.empty => throw Unmarshaller.NoContentException
        case byteString => jawn.parseByteBuffer(byteString.asByteBuffer).toChecked.orThrow
      }
}
