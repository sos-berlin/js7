package com.sos.jobscheduler.master.web.master

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.{passIf, pathSegments}
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.master.web.master.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait MasterRoute extends ApiRoute with WebLogDirectives with TestRoute {

  final val masterRoute: Route =
    pathSegments("api") {
      handleError {
        apiRoute
      }
    } ~
    pathSegments("TEST") {
      passIf(config.getBoolean("jobscheduler.webserver.test")) {
        testRoute
      }
    }
}
