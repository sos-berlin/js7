package com.sos.jobscheduler.agent.web.views

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
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
