package com.sos.jobscheduler.master.web

import akka.actor.ActorRefFactory
import com.sos.jobscheduler.master.web.api.ApiRoute
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
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
