package com.sos.jobscheduler.agent.web.views

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import monix.eval.Task

/**
 * @author Joacim Zschimmer
 */
trait RootWebService extends AgentRouteProvider {

  protected def agentOverview: Task[AgentOverview]

  private implicit def implicitScheduler = scheduler

  protected final lazy val apiRootRoute: Route =
    pathEnd {
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          complete {
            agentOverview
          }
        }
      }
    }
}
