package com.sos.jobscheduler.master.web.master

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.master.web.master.RouteService.RouteServiceRoute
import com.sos.jobscheduler.master.web.master.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait MasterRoute extends ApiRoute with RouteServiceRoute with WebLogDirectives {

  final val masterRoute: Route =
    pathSegments("api") {
      handleError {
        apiRoute
      }
    } ~
    routeServiceRoute
}
