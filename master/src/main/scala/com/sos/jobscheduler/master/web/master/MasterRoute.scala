package com.sos.jobscheduler.master.web.master

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.{passIf, pathSegment}
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.master.web.master.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait MasterRoute extends ApiRoute with WebLogDirectives with TestRoute {

  final val masterRoute: Route =
    pathSegment("api") {
      seal {
        apiRoute
      }
    } ~
    pathSegment("TEST") {
      passIf(config.getBoolean("jobscheduler.webserver.test")) {
        testRoute
      }
    }
}
