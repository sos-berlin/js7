package js7.controller.web.controller.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardDirectives.remainingSegmentOrPath
import js7.common.akkahttp.StandardMarshallers._
import js7.controller.web.common.ControllerRouteProvider
import js7.core.filebased.FileBasedApi
import js7.data.agent.{AgentRef, AgentRefPath}
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait AgentRefRoute extends ControllerRouteProvider
{
  protected def fileBasedApi: FileBasedApi

  private implicit def implicitScheduler: Scheduler = scheduler

  final val agentRefRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathEnd {
          completeTask(
            fileBasedApi.overview[AgentRef])
        } ~
          pathSingleSlash {
            parameter("return".?) {
              case None =>
                completeTask(
                  fileBasedApi.paths[AgentRef])

              case Some("AgentRef") =>
                completeTask(
                  fileBasedApi.fileBaseds[AgentRef])

              case _ =>
                reject
            }
          } ~
          path(remainingSegmentOrPath[AgentRefPath]) { agentRefPath =>
            completeTask(
              fileBasedApi.pathToCurrentFileBased[AgentRef](agentRefPath))
          }
      }
    }
}

object AgentRefRoute {
  intelliJuseImport(() => checkedToResponseMarshaller(null))
}
