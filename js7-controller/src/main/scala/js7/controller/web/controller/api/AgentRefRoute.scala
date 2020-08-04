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
import js7.core.item.InventoryItemApi
import js7.data.agent.{AgentRef, AgentRefPath}
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait AgentRefRoute extends ControllerRouteProvider
{
  protected def itemApi: InventoryItemApi

  private implicit def implicitScheduler: Scheduler = scheduler

  final val agentRefRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathEnd {
          completeTask(
            itemApi.overview[AgentRef])
        } ~
          pathSingleSlash {
            parameter("return".?) {
              case None =>
                completeTask(
                  itemApi.paths[AgentRef])

              case Some("AgentRef") =>
                completeTask(
                  itemApi.items[AgentRef])

              case _ =>
                reject
            }
          } ~
          path(remainingSegmentOrPath[AgentRefPath]) { agentRefPath =>
            completeTask(
              itemApi.pathToCurrentItem[AgentRef](agentRefPath))
          }
      }
    }
}

object AgentRefRoute {
  intelliJuseImport(() => checkedToResponseMarshaller(null))
}
