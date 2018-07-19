package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardDirectives.remainingSegmentOrPath
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.execution.Scheduler

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
          complete(fileBasedApi.overview[Agent])
        } ~
          pathSingleSlash {
            parameter("return".?) {
              case None ⇒
                complete(fileBasedApi.paths[Agent])

              case Some("Agent") ⇒
                complete(fileBasedApi.fileBaseds[Agent])

              case _ ⇒
                reject
            }
          } ~
          path(remainingSegmentOrPath[AgentPath]) { agentPath ⇒
            complete(fileBasedApi.pathToCurrentFileBased[Agent](agentPath))
          }
      }
    }
}

object AgentRoute {
  intelliJuseImport(() ⇒ checkedToResponseMarshaller(null))
}
