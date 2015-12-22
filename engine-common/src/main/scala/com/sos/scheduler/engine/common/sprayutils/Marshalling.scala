package com.sos.scheduler.engine.common.sprayutils

import akka.actor.ActorRefFactory
import spray.http.HttpResponse
import spray.http.StatusCodes._
import spray.httpx.marshalling._

/**
  * @author Joacim Zschimmer
  */
object Marshalling {
  def marshalToHttpResponse[A](implicit marshaller: Marshaller[A], refFactory: ActorRefFactory): A ⇒ HttpResponse =
    result ⇒
      marshalToEntityAndHeaders(result) match {
        case Left(t) ⇒ throw t
        case Right((entity, headers)) ⇒ HttpResponse(OK, entity, headers.toList)
      }
}
