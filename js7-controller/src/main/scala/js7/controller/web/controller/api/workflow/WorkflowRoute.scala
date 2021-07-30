package js7.controller.web.controller.api.workflow

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonSupport._
import js7.common.akkahttp.StandardDirectives.remainingItemPath
import js7.controller.web.common.ControllerRouteProvider
import js7.core.item.VersionedItemApi
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait WorkflowRoute extends ControllerRouteProvider {

  protected def itemApi: VersionedItemApi

  private implicit def implicitScheduler: Scheduler = scheduler

  final val workflowRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathEnd {
          completeTask(
            itemApi.overview[Workflow])
        } ~
        pathSingleSlash {
          parameter("return".?) {
            case None =>
              completeTask(
                itemApi.paths[Workflow])

            case Some("Workflow") =>
              completeTask(
                itemApi.items[Workflow])

            case _ =>
              reject
          }
        } ~
        path(remainingItemPath[WorkflowPath]) { workflowPath =>
          completeTask(
            itemApi.pathToCurrentItem[Workflow](workflowPath))
        }
      }
    }
}
