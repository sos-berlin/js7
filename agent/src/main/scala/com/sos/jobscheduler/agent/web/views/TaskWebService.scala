package com.sos.jobscheduler.agent.web.views

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.task.TaskRegister
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
trait TaskWebService extends AgentRouteProvider {

  protected val taskRegister: TaskRegister
  protected implicit def executionContext: ExecutionContext

  protected final val taskRoute: Route =
    authorizedUser() { user ⇒
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

object TaskWebService {
  intelliJuseImport(() ⇒ jsonOrYamlMarshaller(null))
}
