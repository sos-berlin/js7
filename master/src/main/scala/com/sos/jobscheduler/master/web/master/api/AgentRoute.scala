package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.master.FileBasedApi
import com.sos.jobscheduler.master.order.agent.Agent
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
trait AgentRoute
{
  protected def fileBasedApi: FileBasedApi

  def agentRoute: Route =
    get {
      pathEnd {
        completeTask(fileBasedApi.overview[Agent])
      } ~
        pathSingleSlash {
          parameter("return".?) {
            case None ⇒
              completeTask(fileBasedApi.paths[Agent])

            case Some("Agent") ⇒
              completeTask(fileBasedApi.fileBaseds[Agent])

            case _ ⇒
              reject
          }
        } ~
        path(Segment) { pathString ⇒
          completeTask(fileBasedApi.fileBased[Agent](AgentPath(s"/$pathString")))
        } ~
        extractUnmatchedPath {
          case Uri.Path.Slash(tail) if !tail.isEmpty ⇒    // Slashes not escaped
            completeTask(fileBasedApi.fileBased[Agent](AgentPath("/" + tail.toString)))
          case _ ⇒
            reject
        }
    }
}

object AgentRoute {
  intelliJuseImport(() ⇒ checkedToEntityMarshaller(null))
}
