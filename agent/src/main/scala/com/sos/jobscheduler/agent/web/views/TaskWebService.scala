package js7.agent.web.views

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.agent.data.AgentTaskId
import js7.agent.task.TaskRegister
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.ValidUserPermission
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
trait TaskWebService extends AgentRouteProvider {

  protected val taskRegister: TaskRegister
  protected implicit def executionContext: ExecutionContext

  protected final val taskRoute: Route =
    authorizedUser(ValidUserPermission) { _ =>
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
        path(Segment) { idString =>
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
  intelliJuseImport(() => jsonOrYamlMarshaller(null))
}
