package js7.agent.web.views

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import js7.agent.data.views.AgentOverview
import js7.agent.web.common.AgentRouteProvider
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport.*
import monix.eval.Task
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
trait RootWebService extends AgentRouteProvider {

  protected def agentOverview: Task[AgentOverview]

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val apiRootRoute: Route =
    pathEnd {
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          completeTask {
            agentOverview
          }
        }
      }
    }
}
