package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.{ContentType, HttpEntity}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.Logger
import scala.language.implicitConversions
import spray.json._

/**
 * Like spray.httpx.SprayJsonSupport, but marshals as pretty JSON when text/plain or compact JSON when application/json is requested.
 * Unmarshalling is forwarded to spray.httpx.SprayJsonSupport.
 *
 * @author Joacim Zschimmer
 */
object SprayJsonOrYamlSupport {
  private val logger = Logger(getClass)

  implicit def sprayJsonOrYamlUnmarshallerConverter[A](reader: RootJsonReader[A]): FromEntityUnmarshaller[A] =
    sprayJsonOrYamlUnmarshaller(reader)

  implicit def sprayJsonOrYamlUnmarshaller[A: RootJsonReader]: FromEntityUnmarshaller[A] =
    SprayJsonSupport.sprayJsonUnmarshaller

  implicit def sprayJsonOrYamlMarshallerConverter[A](writer: RootJsonWriter[A]): ToEntityMarshaller[A] =
    sprayJsonOrYamlMarshaller[A](writer)

  implicit def sprayJsonOrYamlMarshaller[A](implicit writer: RootJsonWriter[A]): ToEntityMarshaller[A] =
    Marshaller.oneOf(yamlMarshaller, jsonMarshaller)  // Accept: */* should return YAML (for command line curl)

  private def jsonMarshaller[A: RootJsonWriter]: ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(`application/json`) { value ⇒
      val string = CompactPrinter(implicitly[RootJsonWriter[A]].write(value))
      HttpEntity.Strict(`application/json`, ByteString(string))
    }

  private def yamlMarshaller[A: RootJsonWriter]: ToEntityMarshaller[A] =
    Marshaller.withOpenCharset(`text/plain`) { (value, charset) ⇒
      // Try to reduce to hold two big objects in memory
      def jsValue = implicitly[RootJsonWriter[A]].write(value)
      def yamlValue = YamlJsonConversion.jsValueToYaml(jsValue)
      val string = YamlJsonConversion.yaml.dump(yamlValue)
      HttpEntity.Strict(ContentType(`text/plain`, charset), ByteString(string.getBytes(charset.nioCharset)))
    }

  private[akkahttp] def jsonOrYamlToString[A: RootJsonWriter](value: A, contentType: ContentType): String =
    try {
      def jsValue = implicitly[RootJsonWriter[A]].write(value)
      contentType match {
        case ContentType(`text/plain`, _) ⇒
          // Try to reduce to hold two big objects in memory
          val y = YamlJsonConversion.jsValueToYaml(jsValue)
          YamlJsonConversion.yaml.dump(y)
        case `application/json` ⇒
          CompactPrinter(jsValue)
      }
    } catch { case e: OutOfMemoryError ⇒
        logger.error(e.toString)
        throw new RuntimeException(e.toString, e)  // Too avoid termination of Akka
    }
}
