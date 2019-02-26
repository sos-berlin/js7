package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
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
with CommandRoute
with GraphqlRoute
with OrderRoute
with WorkflowRoute
with AgentRefRoute
with AgentProxyRoute
with SessionRoute
{
  final val apiRoute: Route =
    respondWithHeaders(StandardResponseHeaders: _*) {
      pathEnd {
        apiRootRoute
      } ~
      pathPrefix(Segment) {
        case "session"     ⇒ sessionRoute
        case "event"       ⇒ eventRoute
        case "command"     ⇒ commandRoute
        case "fatEvent"    ⇒ fatEventRoute
        case "order"       ⇒ orderRoute
        case "workflow"    ⇒ workflowRoute
        case "agent"       ⇒ agentRefRoute
        case "agent-proxy" ⇒ agentProxyRoute
        case "graphql"     ⇒ graphqlRoute
        case _ ⇒ complete(NotFound)
      }
    }
}

object ApiRoute {
  private val StandardResponseHeaders = Array(
    `Cache-Control`(`max-age`(0), `no-store`, `no-cache`))
}
