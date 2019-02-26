package com.sos.jobscheduler.master.web.master.api.workflow

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.remainingSegmentOrPath
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
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
