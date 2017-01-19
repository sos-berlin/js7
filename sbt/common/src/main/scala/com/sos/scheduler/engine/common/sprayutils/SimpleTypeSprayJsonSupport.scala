package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.base.utils.ScalaUtils.cast
import java.nio.charset.StandardCharsets.UTF_8
import scala.language.implicitConversions
import scala.reflect.ClassTag
import spray.http.MediaTypes._
import spray.http.{ContentTypes, HttpEntity}
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object SimpleTypeSprayJsonSupport {

  implicit def booleanSprayJsonMarshaller(implicit writer: JsonWriter[JsValue]) = valueMarshaller[JsBoolean]

  implicit def numberSprayJsonMarshaller(implicit writer: JsonWriter[JsValue]) = valueMarshaller[JsNumber]

  implicit def stringSprayJsonMarshaller(implicit writer: JsonWriter[JsValue]) = valueMarshaller[JsString]

  private def valueMarshaller[A <: JsValue]: Marshaller[A] =
    Marshaller.delegate[A, String](ContentTypes.`application/json`) { (value, contentType) ⇒
      CompactPrinter(value)
    }

  //implicit val JsValueUnmarshaller = valueUnmarshaller[JsValue]
  implicit val JsBooleanUnmarshaller = valueUnmarshaller[JsBoolean]
  implicit val JsNumberUnmarshaller = valueUnmarshaller[JsNumber]
  implicit val JsStringUnmarshaller = valueUnmarshaller[JsString]

  private def valueUnmarshaller[A <: JsValue : ClassTag]: Unmarshaller[A] =
    Unmarshaller[A](`application/json`) {
        case HttpEntity.NonEmpty(contentType, data) ⇒ cast[A](JsonParser(ParserInput(data.asString(UTF_8))))
      }
}
