package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.{ContentType, HttpEntity}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.implicits.CompactPrinter
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.CirceToYaml._
import com.sos.jobscheduler.common.http.{CirceJsonSupport, CirceToYaml}
import com.sos.jobscheduler.common.scalautil.Logger
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import scala.util.control.NonFatal

/**
 * Like CirceJsonSupport, but marshals as pretty JSON when text/plain or compact JSON when application/json is requested.
 * Unmarshalling is forwarded to CirceJsonSupport.
 *
 * @author Joacim Zschimmer
 */
object CirceJsonOrYamlSupport
{
  private val logger = Logger(getClass)

  implicit def jsonUnmarshaller[A: Decoder]: FromEntityUnmarshaller[A] =
    CirceJsonSupport.unmarshaller[A]

  implicit def jsonOrYamlMarshaller[A: Encoder]: ToEntityMarshaller[A] =
    Marshaller.oneOf(yamlMarshaller, jsonMarshaller)  // Accept: */* should return YAML (for command line curl)

  private def jsonMarshaller[A](implicit encoder: Encoder[A]): ToEntityMarshaller[A] =
    Marshaller.withFixedContentType(`application/json`) { value =>
      val string = logException(s"jsonMarhaller(${encoder.getClass.getName})") {
        CompactPrinter.pretty(value.asJson)
      }
      HttpEntity.Strict(`application/json`, ByteString(string))
    }

  private def yamlMarshaller[A](implicit encoder: Encoder[A]): ToEntityMarshaller[A] =
    Marshaller.withOpenCharset(`text/plain`) { (value, charset) =>
      try {
        val string = logException(s"yamlMarhaller(${encoder.getClass.getName})") {
          CirceToYaml.yaml.dump(jsonToYaml(value.asJson))   // OutOfMemoryError possible: two big objects in memory
        }
        HttpEntity.Strict(ContentType(`text/plain`, charset), ByteString(string.getBytes(charset.nioCharset)))
      } catch {
        case t: OutOfMemoryError =>
          logger.error(t.toString)
          throw new RuntimeException(s"Error while converting to YAML: $t", t)  // To avoid termination of Akka
      }
    }

  private def logException[A](what: => String)(body: => A): A =
    try body
    catch { case NonFatal(t) =>
      logger.warn(s"jsonMarhaller($what: ${t.toStringWithCauses}", t)
      throw t
    }
}
