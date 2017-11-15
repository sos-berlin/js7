package com.sos.jobscheduler.master.web

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes.TemporaryRedirect
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.master.web.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait AllRoute extends ApiRoute {

  protected implicit def actorRefFactory: ActorRefFactory

  val allRoutes: Route =
    (decodeRequest & encodeResponse) {
      pathSegments("master") {
        masterRoute
      } ~
      pathEndOrSingleSlash {
        htmlPreferred {
          redirect("/master/", TemporaryRedirect)
        } ~
        redirect("/master/api", TemporaryRedirect)
      }
    }

  private def masterRoute: Route =
    pathSegments("api") {
      apiRoute
    }
}
