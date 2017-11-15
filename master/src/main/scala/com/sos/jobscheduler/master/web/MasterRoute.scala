package com.sos.jobscheduler.master.web

import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.master.web.api.ApiRoute

/**
  * @author Joacim Zschimmer
  */
trait MasterRoute extends ApiRoute {

  final val masterRoute: Route =
    pathSegments("api") {
      apiRoute
    }
}
