package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import java.nio.charset.StandardCharsets.UTF_8
import scala.language.implicitConversions
import scala.reflect.ClassTag
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object SimpleTypeSprayJsonSupport {

  private implicit val jsonWriter: JsonWriter[JsValue] =
    new JsonWriter[JsValue] {
      def write(jsValue: JsValue) = jsValue
    }

  implicit val booleanSprayJsonMarshaller: ToEntityMarshaller[JsBoolean] =
    jsValueMarshaller[JsBoolean]

  implicit val numberSprayJsonMarshaller: ToResponseMarshaller[JsNumber] =
    jsValueMarshaller[JsNumber]

  implicit val stringSprayJsonMarshaller: ToEntityMarshaller[JsString] =
    jsValueMarshaller[JsString]

  def jsValueMarshaller[A <: JsValue]: ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(`application/json`) { value ⇒
      HttpEntity(`application/json`, value.compactPrint.getBytes(UTF_8))
    }

  implicit val JsBooleanUnmarshaller = jsValueUnmarshaller[JsBoolean]
  implicit val JsNumberUnmarshaller = jsValueUnmarshaller[JsNumber]
  implicit val JsStringUnmarshaller = jsValueUnmarshaller[JsString]

  private def jsValueUnmarshaller[A <: JsValue: ClassTag]: FromEntityUnmarshaller[A] =
    for (byteString ← Unmarshaller.byteStringUnmarshaller.forContentTypes(`application/json`)) yield
      cast[A](byteString.utf8String.parseJson)
}
