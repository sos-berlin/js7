package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{RawHeader, `Cache-Control`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRoute
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.ApiRoute._
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRoute
import com.sos.jobscheduler.master.web.master.api.graphql.GraphqlRoute
import com.sos.jobscheduler.master.web.master.api.order.OrderRoute
import com.sos.jobscheduler.master.web.master.api.workflow.WorkflowRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends MasterRouteProvider
with ApiRootRoute
with EventRoute
with FatEventRoute
with GraphqlRoute
with OrderRoute
with WorkflowRoute
with AgentRoute
with AgentProxyRoute
with SessionRoute
{
  final val apiRoute: Route =
    respondWithHeaders(StandardResponseHeaders: _*) {
      pathEnd {
        apiRootRoute
      } ~
      pathSegments("session") {
        sessionRoute
      } ~
      pathSegments("event") {
        eventRoute
      } ~
      pathSegments("fatEvent") {
        fatEventRoute
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
      } ~
      pathSegments("graphql") {
        graphqlRoute
      }
    }
}

object ApiRoute {
  private val StandardResponseHeaders = Array(
    RawHeader("X-JobScheduler-Build-ID", BuildInfo.buildId),
    `Cache-Control`(`max-age`(0), `no-store`, `no-cache`))
}
