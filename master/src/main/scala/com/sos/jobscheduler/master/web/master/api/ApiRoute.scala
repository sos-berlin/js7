package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{RawHeader, `Cache-Control`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.master.web.master.api.order.OrderRoute
import com.sos.jobscheduler.master.web.master.api.workflow.WorkflowRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute extends ApiRootRoute with EventRoute with OrderRoute with WorkflowRoute with AgentRoute with AgentProxyRoute {

  val apiRoute: Route =
    respondWithHeader(RawHeader("X-JobScheduler-Build-ID", BuildInfo.buildId)) {
      respondWithHeader(`Cache-Control`(`max-age`(0), `no-store`, `no-cache`)) {
        pathEnd {
          apiRootRoute
        } ~
        pathSegments("event") {
          eventRoute
        } ~
        pathSegments("order") {
          orderRoute
        } ~
        pathSegments("workflow") {
          workflowRoute
        } ~
        pathSegments("agent") {
          agentRoute
        } ~
        pathSegments("agent-proxy") {
          agentProxyRoute
        }
      }
    }
}
