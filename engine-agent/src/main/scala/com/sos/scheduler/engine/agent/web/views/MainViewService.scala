package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait MainViewService extends ServiceStandards {

  protected def agentOverview: AgentOverview

  addRoute {
    (get & pathPrefix("agent")) {
      path("overview") {
        complete { agentOverview }
      }
    }
  }
}
