package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait ProcessHandlerViewService extends ServiceStandards {

  protected def processHandlerView: ProcessHandlerView

  addRoute {
    (get & pathPrefix("agent" / "processHandler")) {
      complete { processHandlerView }
    }
  }
}
