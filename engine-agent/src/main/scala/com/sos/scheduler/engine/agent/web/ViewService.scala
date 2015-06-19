package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.marshal.SprayJsonOrYamlSupport._
import java.nio.file.{Files, Paths}
import spray.http.StatusCodes.{NotFound, OK}
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait ViewService extends ServiceStandards {

  protected def processHandlerView: ProcessHandlerView
  protected def agentOverview: AgentOverview

  final def viewRoute =
    agentStandard {
      (get & pathPrefix("agent")) {
        path("fileStatus") {
          parameter("file") { path ⇒
            complete {
              val file = Paths.get(path)
              if (Files.exists(file)) OK else NotFound
            }
          }
        } ~
        path("overview") {
          complete { agentOverview }
        } ~
        path("processHandler") {
          complete { processHandlerView }
        }
      }
    }
}
