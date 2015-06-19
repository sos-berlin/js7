package com.sos.scheduler.engine.agent.web.marshal

import scala.language.implicitConversions
import spray.http.ContentTypes.{`application/json`, `text/plain(UTF-8)`, `text/plain`}
import spray.httpx.SprayJsonSupport
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller
import spray.json._

/**
 * Like spray.httpx.SprayJsonSupport, but marshals as pretty JSON when text/plain or compact JSON when application/json is requested.
 * Unmarshalling is forwarded to spray.httpx.SprayJsonSupport.
 *
 * @author Joacim Zschimmer
 */
object SprayJsonOrYamlSupport {

  implicit def sprayJsonOrYamlUnmarshallerConverter[T](reader: RootJsonReader[T]): Unmarshaller[T] =
    sprayJsonOrYamlUnmarshaller(reader)

  implicit def sprayJsonOrYamlUnmarshaller[T: RootJsonReader] =
    SprayJsonSupport.sprayJsonUnmarshaller

  implicit def sprayJsonOrYamlMarshallerConverter[T](writer: RootJsonWriter[T]): Marshaller[T] =
    sprayJsonOrYamlMarshaller[T](writer)

  implicit def sprayJsonOrYamlMarshaller[T](implicit writer: RootJsonWriter[T]) =
    Marshaller.delegate[T, String](`text/plain`, `application/json`) { (value, contentType) ⇒
      val json = writer.write(value)
      contentType match {
        case `text/plain` | `text/plain(UTF-8)` ⇒ YamlPrinter(json)
        case _ ⇒ CompactPrinter(json)
      }
    }
}
