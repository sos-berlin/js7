package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType}
import akka.util.ByteString
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object StandardMarshallers
{
  private val ProblemStatusCode = BadRequest
  private val Nl = ByteString("\n")

  implicit val problemToEntityMarshaller: ToEntityMarshaller[Problem] =
    Marshaller.oneOf(
      stringMarshaller(`text/plain`, _.toString),
      jsonOrYamlMarshaller[Problem])

  implicit val problemToResponseMarshaller: ToResponseMarshaller[Problem] =
    problemToEntityMarshaller map (entity ⇒ HttpResponse.apply(ProblemStatusCode, Nil, entity))

  def stringMarshaller[A](mediaType: MediaType.WithOpenCharset, toString: A ⇒ String): ToEntityMarshaller[A] =
    Marshaller.withOpenCharset(mediaType) { (a, charset) ⇒
      var byteString = ByteString(toString(a), charset.nioCharset)
      if (!byteString.endsWith(Nl)) byteString ++= Nl   // Append \n to terminate curl output properly
      HttpEntity.Strict(ContentType(mediaType, charset), byteString)
    }

  implicit def checkedToEntityMarshaller[A: ToResponseMarshaller]: ToResponseMarshaller[Checked[A]] =
    Marshaller {
      implicit ec ⇒ {
        case Valid(a) ⇒
          implicitly[ToResponseMarshaller[A]].apply(a)
        case Invalid(problem) ⇒
          problemToResponseMarshaller(problem)
      }
    }
}
