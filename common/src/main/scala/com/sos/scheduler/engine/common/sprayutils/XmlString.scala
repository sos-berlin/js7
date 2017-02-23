package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.xml.XmlUtils.xmlByteStringToString
import spray.http.HttpCharsets._
import spray.http.MediaTypes._
import spray.http.{ContentType, HttpEntity}
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller

/**
  * @see RFC 3023 XML Media Types, https://tools.ietf.org/html/rfc3023
  * @author Joacim Zschimmer
 */
final case class XmlString(string: String)

object XmlString {
  implicit val marshaller: Marshaller[XmlString] = Marshaller.of[XmlString](`application/xml`, `text/xml`) {
    (value, requestedContentType, ctx) ⇒
      val contentType =
        if (requestedContentType.definedCharset.isDefined)
          requestedContentType
        else
          requestedContentType withCharset `UTF-8`  // Original contentType.charset defaults to ISO-8859-1. For sending, we prefer UTF-8
      ctx.marshalTo(HttpEntity(contentType, value.string.getBytes(contentType.charset.nioCharset)))
  }

  implicit val unmarshaller: Unmarshaller[XmlString] = Unmarshaller[XmlString](`application/xml`, `text/xml`) {
    case HttpEntity.NonEmpty(contentType @ ContentType(`text/xml`, _), data) ⇒
      // Using given or default text/xml charset
      XmlString(data.asString(contentType.charset.nioCharset))

    case HttpEntity.NonEmpty(ContentType(`application/xml`, Some(charset)), data) ⇒
      XmlString(data.asString(charset))

    case HttpEntity.NonEmpty(ContentType(`application/xml`, None), data) ⇒
      // No charset given. The encoding has to be extracted from XML document itself.
      XmlString(xmlByteStringToString(data.toByteString))
  }
}
