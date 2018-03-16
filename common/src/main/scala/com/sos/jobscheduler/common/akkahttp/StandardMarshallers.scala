package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType}
import akka.util.ByteString
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object StandardMarshallers
{
  private val ProblemStatusCode = BadRequest

  implicit val problemToEntityMarshaller: ToEntityMarshaller[Problem] =
    stringMarshaller(`text/plain`, _.toString)

  implicit val problemToResponseMarshaller: ToResponseMarshaller[Problem] =
    problemToEntityMarshaller map (entity ⇒ HttpResponse.apply(ProblemStatusCode, Nil, entity))

  def stringMarshaller[A](mediaType: MediaType.WithOpenCharset, toString: A ⇒ String): ToEntityMarshaller[A] =
    Marshaller.withOpenCharset(mediaType) { (a, charset) ⇒
      HttpEntity.Strict(
        ContentType(mediaType, charset),
        ByteString(toString(a).getBytes(charset.nioCharset)))
    }

  implicit def checkedToEntityMarshaller[A: ToEntityMarshaller]: ToResponseMarshaller[Checked[A]] =
    Marshaller {
      implicit ec ⇒ {
        case Valid(a) ⇒
          implicitly[ToResponseMarshaller[A]].apply(a)
        case Invalid(problem) ⇒
          problemToResponseMarshaller(problem)
      }
    }

  //implicit def XXcheckedToEntityMarshaller[A: ToEntityMarshaller](checked: Checked[A]): ToResponseMarshaller[Checked[A]] =
  //  new Marshaller[Checked[A], HttpResponse] {
  //    def apply(checked: Checked[A])(implicit ec: ExecutionContext): Future[List[Marshalling[HttpResponse]]] =
  //      checked match {
  //      case Valid(a) ⇒ implicitly[A](a)
  //      case Invalid(problem) ⇒ problemToResponseMarshaller(problem)
  //      }
  //  }
}
