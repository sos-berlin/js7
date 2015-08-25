package com.sos.scheduler.engine.common.sprayutils

import SimpleTypeSprayJsonSupport._
import com.sos.scheduler.engine.common.scalautil.Logger
import scala.collection.{immutable, mutable}
import spray.http.ContentTypes._
import spray.httpx.SprayJsonSupport
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling._
import spray.json.{RootJsonWriter, RootJsonReader}
import scala.language.implicitConversions

/**
 * @author Joacim Zschimmer
 */
object SimpleTypeSprayJsonSupport {
  implicit def sprayJsonOrYamlUnmarshallerConverter[T](reader: RootJsonReader[T]): Unmarshaller[T] =
    sprayJsonOrYamlUnmarshaller(reader)

  implicit def sprayJsonOrYamlUnmarshaller[T: RootJsonReader] =
    SprayJsonSupport.sprayJsonUnmarshaller

  implicit def sprayJsonOrYamlMarshallerConverter[T](writer: RootJsonWriter[T]): Marshaller[T] =
    sprayJsonOrYamlMarshaller[T](writer)

  implicit def booleanSprayJsonMarshaller[T](implicit writer: RootJsonWriter[T]) =
    Marshaller.delegate[T, String](`text/plain`, `application/json`) { (value, contentType) ⇒
      val json = writer.write(value)
      contentType match {
        case `text/plain` | `text/plain(UTF-8)` ⇒ YamlPrinter(json)
        case _ ⇒ CompactPrinter(json)
      }
    }
}

object SprayJsonSimpleTypeSupport {
  private val logger = Logger(getClass)
}
