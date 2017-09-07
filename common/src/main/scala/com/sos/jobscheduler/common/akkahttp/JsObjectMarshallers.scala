package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import java.nio.charset.StandardCharsets.UTF_8
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object JsObjectMarshallers {
  // Needed for AgentWebServiceTest.

  implicit val JsObjectMarshaller: ToEntityMarshaller[JsObject] =
    Marshaller.withFixedContentType(`application/json`) { value ⇒
      HttpEntity(`application/json`, value.compactPrint.getBytes(UTF_8))
    }

  implicit val JsObjectUnmarshaller: FromEntityUnmarshaller[JsObject] =
    for (byteString ← Unmarshaller.byteStringUnmarshaller.forContentTypes(`application/json`)) yield
      byteString.utf8String.parseJson.asJsObject

  implicit val JsArrayMarshaller: ToEntityMarshaller[JsArray] =
    Marshaller.withFixedContentType(`application/json`) { value ⇒
      HttpEntity(`application/json`, value.compactPrint.getBytes(UTF_8))
    }

  implicit val JsArrayUnmarshaller: FromEntityUnmarshaller[JsArray] =
    for (byteString ← Unmarshaller.byteStringUnmarshaller.forContentTypes(`application/json`)) yield
      byteString.utf8String.parseJson match {
        case o: JsArray ⇒ o
        case o ⇒ sys.error(s"JSON array expected instead of ${o.getClass.getSimpleName}")
      }
}
