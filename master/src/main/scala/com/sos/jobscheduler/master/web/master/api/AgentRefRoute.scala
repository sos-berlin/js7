package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.remainingSegmentOrPath
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait AgentRefRoute extends MasterRouteProvider
{
  protected def fileBasedApi: FileBasedApi

  private implicit def implicitScheduler: Scheduler = scheduler

  final val agentRefRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ ⇒
        pathEnd {
          completeTask(
            fileBasedApi.overview[AgentRef])
        } ~
          pathSingleSlash {
            parameter("return".?) {
              case None ⇒
                completeTask(
                  fileBasedApi.paths[AgentRef])

              case Some("AgentRef") ⇒
                completeTask(
                  fileBasedApi.fileBaseds[AgentRef])

              case _ ⇒
                reject
            }
          } ~
          path(remainingSegmentOrPath[AgentRefPath]) { agentRefPath ⇒
            completeTask(
              fileBasedApi.pathToCurrentFileBased[AgentRef](agentRefPath))
          }
      }
    }
}

object AgentRefRoute {
  intelliJuseImport(() ⇒ checkedToResponseMarshaller(null))
}
