package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse, MediaType}
import akka.util.ByteString
import com.sos.jobscheduler.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
object StandardMarshallers {
  implicit val problemToEntityMarshaller: ToEntityMarshaller[Problem] =
    stringMarshaller(`text/plain`, _.toString)

  implicit val problemToResponseMarshaller: ToResponseMarshaller[Problem] =
    problemToEntityMarshaller map (entity ⇒ HttpResponse.apply(BadRequest, Nil, entity))

  def stringMarshaller[A](mediaType: MediaType.WithOpenCharset, toString: A ⇒ String): ToEntityMarshaller[A] =
    Marshaller.withOpenCharset(mediaType) { (a, charset) ⇒
      HttpEntity.Strict(
        ContentType(mediaType, charset),
        ByteString(toString(a).getBytes(charset.nioCharset)))
    }
}
