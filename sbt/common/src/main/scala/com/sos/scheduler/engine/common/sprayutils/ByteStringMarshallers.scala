package com.sos.scheduler.engine.common.sprayutils

import akka.util.ByteString
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller

/**
 * @author Joacim Zschimmer
 */
object ByteStringMarshallers {
  implicit val ByteStringMarshaller = Marshaller.of[ByteString](`application/octet-stream`) {
    (value, contentType, ctx) ⇒ ctx.marshalTo(HttpEntity(contentType, value))
  }

  implicit val ByteStringUnmarshaller = Unmarshaller[ByteString](`application/octet-stream`) {
    case HttpEntity.NonEmpty(contentType, entity) ⇒ entity.toByteString
  }
}
