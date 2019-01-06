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
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider

/**
  * @author Joacim Zschimmer
  */
trait AgentRoute extends MasterRouteProvider
{
  protected def fileBasedApi: FileBasedApi

  private implicit def implicitScheduler = scheduler

  final val agentRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ ⇒
        pathEnd {
          completeTask(
            fileBasedApi.overview[Agent])
        } ~
          pathSingleSlash {
            parameter("return".?) {
              case None ⇒
                completeTask(
                  fileBasedApi.paths[Agent])

              case Some("Agent") ⇒
                completeTask(
                  fileBasedApi.fileBaseds[Agent])

              case _ ⇒
                reject
            }
          } ~
          path(remainingSegmentOrPath[AgentPath]) { agentPath ⇒
            completeTask(
              fileBasedApi.pathToCurrentFileBased[Agent](agentPath))
          }
      }
    }
}

object AgentRoute {
  intelliJuseImport(() ⇒ checkedToResponseMarshaller(null))
}
