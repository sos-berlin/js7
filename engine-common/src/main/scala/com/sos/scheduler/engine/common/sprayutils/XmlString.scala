package com.sos.scheduler.engine.common.sprayutils

import java.nio.charset.StandardCharsets.UTF_8
import spray.http.HttpCharsets.`UTF-8`
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller

/**
 * @author Joacim Zschimmer
 */
final case class XmlString(string: String)

object XmlString {
  implicit val marshaller: Marshaller[XmlString] = Marshaller.of[XmlString](`application/xml`, `text/xml`) {
    (value, contentType, ctx) ⇒
      val data = value.string.getBytes(UTF_8)
      val entity = (contentType.mediaType: @unchecked) match {
        case `application/xml` ⇒ HttpEntity(`application/xml`, data)
        case `text/xml` ⇒ HttpEntity(`text/xml` withCharset `UTF-8`, data)
      }
      ctx.marshalTo(entity)
  }

  implicit val unmarshaller: Unmarshaller[XmlString] = Unmarshaller[XmlString](`application/xml`, `text/xml`) {
    case HttpEntity.NonEmpty(contentType, data) ⇒
      (contentType.mediaType: @unchecked) match {
        case `application/xml` ⇒ XmlString(data.asString(UTF_8))
        case `text/xml` ⇒ XmlString(data.asString(contentType.charset.nioCharset))
      }
  }
}
