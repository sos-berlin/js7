package com.sos.jobscheduler.agent.web.views

import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.task.TaskRegister
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.common.utils.IntelliJUtils._
import scala.concurrent.ExecutionContext
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait TaskWebService extends AgentWebService {

  intelliJuseImports(rootFormat _)

  protected val taskRegister: TaskRegister
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { _ ⇒
    pathSegments("task") {
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        pathEnd {
          get {
            complete {
              taskRegister.overview
            }
          }
        } ~
        pathSingleSlash {
          get {
            complete {
              taskRegister.taskOverviews map { _ sortBy { _.taskId.index } }
            }
          }
        } ~
        path(Segment) { idString ⇒
          val agentTaskId = AgentTaskId(idString)
          get {
            complete {
              taskRegister.taskOverview(agentTaskId)
            }
          }
        }
      }
    }
  }
}
