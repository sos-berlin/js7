package com.sos.scheduler.engine.master.web

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.master.web.api.ApiRoute
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import spray.http.StatusCodes.TemporaryRedirect
import spray.routing.Directives._
import spray.routing.Route

/**
  * @author Joacim Zschimmer
  */
trait AllRoute extends ApiRoute {

  protected implicit def actorRefFactory: ActorRefFactory

  def allRoutes: Route =
    (decompressRequest() & compressResponseIfRequested(())) {
      pathSegments("jobscheduler") {
        pathSegments("master") {
          masterRoute
        }
      } ~
      pathEndOrSingleSlash {
        redirect("/jobscheduler/master/api", TemporaryRedirect)  // TODO Only if htmlPreferred
      }
    }

  private def masterRoute: Route =
    pathSegments("api") {
      apiRoute
    }
}
