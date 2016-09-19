package com.sos.scheduler.engine.common.sprayutils

import java.nio.charset.StandardCharsets.UTF_8
import spray.http.HttpCharsets.`UTF-8`
import spray.http.MediaTypes._
import spray.http.{ContentType, HttpEntity}
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller

/**
 * @author Joacim Zschimmer
 */
final case class JsonString(string: String)

object JsonString {
  implicit val marshaller: Marshaller[JsonString] = Marshaller.of[JsonString](`application/json`) {
    (value, contentType: ContentType, ctx) ⇒
      // Enforce UTF-8 like in XmlString ?
      ctx.marshalTo(HttpEntity(contentType, value.string.getBytes(UTF_8)))
  }

  implicit val unmarshaller: Unmarshaller[JsonString] = Unmarshaller[JsonString](`application/json`) {
    case HttpEntity.NonEmpty(ContentType(`application/json`, charsetOption), data) ⇒
      JsonString(data.asString(charsetOption getOrElse `UTF-8`))
  }
}
