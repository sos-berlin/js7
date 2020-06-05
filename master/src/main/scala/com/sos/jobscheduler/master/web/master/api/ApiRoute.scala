package js7.master.web.master.api

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.web.session.SessionRoute
import js7.master.web.common.MasterRouteProvider
import js7.master.web.master.api.ApiRoute._
import js7.master.web.master.api.fatevent.FatEventRoute
import js7.master.web.master.api.graphql.GraphqlRoute
import js7.master.web.master.api.log.LogRoute
import js7.master.web.master.api.order.OrderRoute
import js7.master.web.master.api.workflow.WorkflowRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends MasterRouteProvider
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
      pathEnd {
        apiRootRoute
      } ~
      pathPrefix(Segment) {
        case "session"     => sessionRoute
        case "event"       => eventRoute
        case "journal"     => journalRoute
        case "command"     => commandRoute
        case "fatEvent"    => fatEventRoute
        case "order"       => orderRoute
        case "workflow"    => workflowRoute
        case "agent"       => agentRefRoute
        case "agent-proxy" => agentProxyRoute
        case "snapshot"    => snapshotRoute
        case "cluster"     => clusterRoute
        case "graphql"     => graphqlRoute
        case "log"         => logRoute
        case _ => complete(NotFound)
      }
    }
}

object ApiRoute {
  private val StandardResponseHeaders = Vector(
    `Cache-Control`(`max-age`(0), `no-store`, `no-cache`))
}
