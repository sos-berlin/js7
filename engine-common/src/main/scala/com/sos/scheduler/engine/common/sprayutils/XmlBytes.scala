package com.sos.scheduler.engine.common.sprayutils

import akka.util.ByteString
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller

/**
  * @author Joacim Zschimmer
  */
final case class XmlBytes(byteString: ByteString)

object XmlBytes {

  implicit val marshaller = Marshaller.of[XmlBytes](`application/xml`) { (value, contentType, ctx) ⇒
    val XmlBytes(b) = value
    ctx.marshalTo(HttpEntity(contentType, b))
  }

  implicit val unmarshaller = Unmarshaller[XmlBytes](`application/xml`) {
    case HttpEntity.NonEmpty(contentType, data) ⇒ XmlBytes(data.toByteString)
  }
}
