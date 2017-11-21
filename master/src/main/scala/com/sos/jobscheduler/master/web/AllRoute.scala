package com.sos.jobscheduler.master.web

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes.TemporaryRedirect
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
        }
      } ~
      pathSegments("master") {
        masterRoute
      }
    }
}
