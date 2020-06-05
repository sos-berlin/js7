package js7.master.web.master.api.workflow

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardDirectives.remainingSegmentOrPath
import js7.common.akkahttp.StandardMarshallers._
import js7.core.filebased.FileBasedApi
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.web.common.MasterRouteProvider
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait WorkflowRoute extends MasterRouteProvider {

  protected def fileBasedApi: FileBasedApi

  private implicit def implicitScheduler: Scheduler = scheduler

  final val workflowRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathEnd {
          completeTask(
            fileBasedApi.overview[Workflow])
        } ~
        pathSingleSlash {
          parameter("return".?) {
            case None =>
              completeTask(
                fileBasedApi.paths[Workflow])

            case Some("Workflow") =>
              completeTask(
                fileBasedApi.fileBaseds[Workflow])

            case _ =>
              reject
          }
        } ~
        path(remainingSegmentOrPath[WorkflowPath]) { workflowPath =>
          completeTask(
            fileBasedApi.pathToCurrentFileBased[Workflow](workflowPath))
        }
      }
    }
}
