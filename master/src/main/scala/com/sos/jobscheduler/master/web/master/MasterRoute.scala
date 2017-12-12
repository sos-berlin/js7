package com.sos.jobscheduler.master.web.master

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.master.web.master.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait MasterRoute extends ApiRoute {

  final val masterRoute: Route =
    pathEndElseRedirect {
      indexHtmlRoute
    } ~
    pathSegments("gui") {
      guiRoute
    } ~
    pathSegments("api") {
      apiRoute
    }
}
