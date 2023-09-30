package js7.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`application/octet-stream`
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.util.ByteString

/**
 * @author Joacim Zschimmer
 */
object ByteStringMarshallers:

  implicit val ByteStringMarshaller: ToEntityMarshaller[ByteString] =
    Marshaller.withFixedContentType(`application/octet-stream`) { byteString =>
      HttpEntity(`application/octet-stream`, byteString)
    }

  implicit val ByteStringUnmarshaller: FromEntityUnmarshaller[ByteString] =
    Unmarshaller.byteStringUnmarshaller.forContentTypes(`application/octet-stream`)
