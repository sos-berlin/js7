package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{RawHeader, `Cache-Control`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.master.web.master.api.order.OrderRoute
import com.sos.jobscheduler.master.web.master.api.workflow.WorkflowRoute
import com.sos.jobscheduler.master.web.master.gui.GuiRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute extends ApiRootRoute with OrderRoute with WorkflowRoute with GuiRoute {

  val apiRoute: Route =
    respondWithHeader(RawHeader("X-JobScheduler-Build-ID", BuildInfo.buildId)) {
      respondWithHeader(`Cache-Control`(`max-age`(0), `no-store`, `no-cache`)) {
        pathEnd {
          apiRootRoute
        } ~
        pathSegments("order") {
          orderRoute
        } ~
        pathSegments("workflow") {
          workflowRoute
        }
      }
    }
}
