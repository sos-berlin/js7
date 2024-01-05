package js7.common.pekkohttp

import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/octet-stream`
import org.apache.pekko.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import org.apache.pekko.util.ByteString

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
