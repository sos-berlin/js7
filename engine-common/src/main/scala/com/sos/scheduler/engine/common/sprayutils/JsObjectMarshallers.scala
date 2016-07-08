package com.sos.scheduler.engine.common.sprayutils

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
  // Needed for AgentWebServiceTest.
  // Spray does not include these methods ???

  implicit val JsObjectMarshaller = Marshaller.of[JsObject](`application/json`) { (value, contentType, ctx) ⇒
    ctx.marshalTo(HttpEntity(`application/json`, value.compactPrint.getBytes(UTF_8)))
  }

  implicit val JsObjectUnmarshaller = Unmarshaller[JsObject](`application/json`) {
    case HttpEntity.NonEmpty(contentType, data) ⇒ data.asString(UTF_8).parseJson.asJsObject
  }

  implicit val JsArrayMarshaller = Marshaller.of[JsArray](`application/json`) { (value, contentType, ctx) ⇒
    ctx.marshalTo(HttpEntity(`application/json`, value.compactPrint.getBytes(UTF_8)))
  }

  implicit val JsArrayUnmarshaller = Unmarshaller[JsArray](`application/json`) {
    case HttpEntity.NonEmpty(contentType, data) ⇒ data.asString(UTF_8).parseJson match {
      case o: JsArray ⇒ o
      case o => sys.error(s"JSON array expected instead of ${o.getClass.getSimpleName}")
    }
  }
}
