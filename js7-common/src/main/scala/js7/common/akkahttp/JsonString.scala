package js7.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.*
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * @author Joacim Zschimmer
 */
final case class JsonString(string: String)

object JsonString {
  implicit val marshaller: ToEntityMarshaller[JsonString] =
    Marshaller.withFixedContentType(`application/json`) { value =>
      HttpEntity(`application/json`, value.string.getBytes(UTF_8))
  }

  implicit val unmarshaller: FromEntityUnmarshaller[JsonString] =
    for (byteString <- Unmarshaller.byteStringUnmarshaller.forContentTypes(`application/json`))
      yield JsonString(byteString.utf8String)
}
