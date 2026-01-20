package js7.common.pekkohttp

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, jawn}
import js7.base.circeutils.CirceUtils.*
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import org.apache.pekko.http.scaladsl.model.ContentTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import org.apache.pekko.util.ByteString
import scala.util.control.NonFatal

object CirceJsonSupport:
  private val logger = Logger[this.type]

  implicit def jsonMarshaller[A](using encoder: Encoder[A]): ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(`application/json`): value =>
      val jsonString = logException(s"jsonMarhaller(${encoder.getClass.getName})"):
        value.asJson.toByteSequence[ByteString]
      HttpEntity(`application/json`, jsonString)

  private def logException[A](what: => String)(body: => A): A =
    try body
    catch case NonFatal(t) =>
      logger.warn(s"jsonMarhaller($what: ${t.toStringWithCauses}", t)
      throw t

  given jsonToEntityMarshaller: ToEntityMarshaller[Json] =
    Marshaller.withFixedContentType(`application/json`): json =>
      HttpEntity(`application/json`, json.toByteSequence[ByteString])

  implicit final def jsonUnmarshaller[A: Decoder]: FromEntityUnmarshaller[A] =
    entityToJsonUnmarshaller.map: json =>
      summon[Decoder[A]]
        .decodeJson(json)
        .toChecked/*renders message*/
        .orThrowWithoutStacktrace

  given entityToJsonUnmarshaller: FromEntityUnmarshaller[Json] =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .map:
        case ByteString.empty => throw Unmarshaller.NoContentException
        case byteString => jawn.parseByteBuffer(byteString.asByteBuffer).toChecked.orThrow
