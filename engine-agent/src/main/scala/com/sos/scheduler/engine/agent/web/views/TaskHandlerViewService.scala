package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.task.TaskHandlerView
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TaskHandlerViewService extends ServiceStandards {

  protected def taskHandlerView: TaskHandlerView

  addApiRoute {
    (pathPrefix("task") & pathEnd & get) {
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        complete { taskHandlerView }
      }
    }
  }
}
