package com.sos.jobscheduler.master.web

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes.{NotFound, TemporaryRedirect}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments

/**
  * @author Joacim Zschimmer
  */
trait AllRoute extends MasterRoute {

  protected implicit def actorRefFactory: ActorRefFactory

  val allRoutes: Route =
    (decodeRequest & encodeResponse) {
      pathEndOrSingleSlash {
        htmlPreferred {
          redirect("/master", TemporaryRedirect)
        } ~
        extractRequest { request ⇒
          complete((NotFound, s"Try ${request.uri.copy(rawQueryString = None).withPath(Uri.Path("/master/api"))}"))
        }
      } ~
      pathSegments("master") {
        masterRoute
      }
    }
}
