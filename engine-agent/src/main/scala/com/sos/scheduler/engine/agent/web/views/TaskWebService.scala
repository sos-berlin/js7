package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.utils.IntelliJUtils._
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TaskWebService extends AgentWebService {

  intelliJuseImports(rootFormat _)

  protected def taskHandlerView: TaskHandlerView

  addApiRoute {
    pathPrefix("task") {
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        pathEnd {
          get {
            complete { taskHandlerView.overview }
          }
        } ~
        pathSingleSlash {
          get {
            complete { taskHandlerView.taskOverviews }
          }
        } ~
        path(Segment) { idString ⇒
          val agentTaskId = AgentTaskId(idString)
          get {
            complete { taskHandlerView.taskOverview(agentTaskId) }
          }
        }
      }
    }
  }
}
