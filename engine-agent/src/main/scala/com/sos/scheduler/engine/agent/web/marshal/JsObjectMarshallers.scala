package com.sos.scheduler.engine.agent.web.marshal

import java.nio.charset.StandardCharsets.UTF_8
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object JsObjectMarshallers {
  implicit val marshaller = Marshaller.of[JsObject](`application/json`) { (value, contentType, ctx) ⇒
    ctx.marshalTo(HttpEntity(`application/json`, value.compactPrint.getBytes(UTF_8)))
  }

  implicit val unmarshaller = Unmarshaller[JsObject](`application/json`) {
    case HttpEntity.NonEmpty(contentType, data) ⇒ data.asString(UTF_8).parseJson.asJsObject
  }
}
