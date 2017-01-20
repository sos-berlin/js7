package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait RootWebService extends AgentWebService {

  protected def agentOverview: AgentOverview

  routeBuilder.addApiRoute {
    (pathEndOrSingleSlash | path("overview")) {
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          complete { agentOverview }
        }
      }
    }
  }
}
