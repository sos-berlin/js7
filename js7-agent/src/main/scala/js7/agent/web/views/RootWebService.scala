package js7.agent.web.views

import org.apache.pekko.http.scaladsl.model.headers.CacheDirectives.`max-age`
import org.apache.pekko.http.scaladsl.model.headers.`Cache-Control`
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import js7.agent.data.views.AgentOverview
import js7.agent.web.common.AgentRouteProvider
import js7.common.pekkohttp.PekkoHttpServerUtils.completeTask
import js7.common.pekkohttp.CirceJsonSupport.*
import monix.eval.Task
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
trait RootWebService extends AgentRouteProvider:

  protected def agentOverview: Task[AgentOverview]

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val apiRootRoute: Route =
    pathEnd:
      get:
        respondWithHeader(`Cache-Control`(`max-age`(0))):
          completeTask:
            agentOverview
