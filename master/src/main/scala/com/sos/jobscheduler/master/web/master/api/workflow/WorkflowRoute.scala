package com.sos.jobscheduler.master.web.master.api.workflow

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
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

  protected implicit def scheduler: Scheduler
  protected def fileBasedApi: FileBasedApi

  final val workflowRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ ⇒
        pathEnd {
          complete(fileBasedApi.overview[Workflow])
        } ~
        pathSingleSlash {
          parameter("return".?) {
            case None ⇒
              complete(fileBasedApi.paths[Workflow])

            case Some("Workflow") ⇒
              complete(fileBasedApi.fileBaseds[Workflow])

            case _ ⇒
              reject
          }
        } ~
        path(remainingSegmentOrPath[WorkflowPath]) { workflowPath ⇒
          complete(fileBasedApi.pathToCurrentFileBased[Workflow](workflowPath))
        }
      }
    }
}
