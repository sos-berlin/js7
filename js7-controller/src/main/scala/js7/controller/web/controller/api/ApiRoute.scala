package js7.controller.web.controller.api

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.web.session.SessionRoute
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.ApiRoute._
import js7.controller.web.controller.api.fatevent.FatEventRoute
import js7.controller.web.controller.api.graphql.GraphqlRoute
import js7.controller.web.controller.api.log.LogRoute
import js7.controller.web.controller.api.order.OrderRoute
import js7.controller.web.controller.api.workflow.WorkflowRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends ControllerRouteProvider
with ApiRootRoute
with EventRoute
with JournalRoute
with FatEventRoute
with CommandRoute
with GraphqlRoute
with OrderRoute
with WorkflowRoute
with AgentRefRoute
with AgentProxyRoute
with SnapshotRoute
with SessionRoute
with ClusterRoute
with LogRoute
{
  final val apiRoute: Route =
    respondWithHeaders(StandardResponseHeaders) {
      pathPrefix(Segment) {
        case "journal"     => journalRoute
        case "event"       => eventRoute
        case "command"     => commandRoute
        case "cluster"     => clusterRoute
        case "fatEvent"    => fatEventRoute
        case "order"       => orderRoute
        case "workflow"    => workflowRoute
        case "agent"       => agentRefRoute
        case "agent-proxy" => agentProxyRoute
        case "snapshot"    => snapshotRoute
        case "session"     => sessionRoute
        case "graphql"     => graphqlRoute
        case "log"         => logRoute
        case _ => complete(NotFound)
      } ~
      pathEnd {
        apiRootRoute
      }
    }
}

object ApiRoute
{
  private val StandardResponseHeaders = Vector(
    `Cache-Control`(`max-age`(0), `no-store`, `no-cache`))
}
