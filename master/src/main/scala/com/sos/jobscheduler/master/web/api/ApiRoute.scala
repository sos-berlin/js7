package com.sos.jobscheduler.master.web.api

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.master.web.api.root.RootRoute
import com.sos.jobscheduler.master.web.simplegui.{FrontEndRoute, GuiRoute}

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute extends RootRoute with OrderRoute with FrontEndRoute with GuiRoute {

  val apiRoute: Route =
    respondWithHeader(RawHeader("X-JobScheduler-Build-ID", BuildInfo.buildId)) {
      pathEnd {
        rootRoute
      } ~
      pathSegments("order") {
        orderRoute
      } ~
      pathSegments("frontend") {
        frontEndRoute
      }
    }
}
