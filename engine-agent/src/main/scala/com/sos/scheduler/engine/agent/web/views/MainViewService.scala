package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait MainViewService extends ServiceStandards {

  protected def agentOverview: AgentOverview

  addApiRoute {
    (pathEndOrSingleSlash | (pathPrefix("overview") & pathEnd) & get) {
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        complete { agentOverview }
      }
    }
  }
}
