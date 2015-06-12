package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.data.responses.Response
import com.sos.scheduler.engine.agent.web.marshal.PrettyOrCompactSprayJsonSupport._
import com.sos.scheduler.engine.agent.web.marshal.ResponseXmlSupport._
import com.sos.scheduler.engine.agent.web.marshal.XmlString
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutor
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait CommandService extends RouteStandards {

  protected def executeCommand(command: Command): Future[Response]

  final def agentCommandRoute =
    agentStandard {
      (post & path("agent" / "command")) {
        entity(as[Command]) { command ⇒
          val future = executeCommand(command)
          onSuccess(future) { response: Response ⇒ complete(response) }
        } ~
        entity(as[XmlString]) { case XmlString(commandXml) ⇒
          val future = CommandXmlExecutor.execute(commandXml) { cmd ⇒ executeCommand(cmd) }
          onSuccess(future) { response ⇒ complete(response) }
        }
      }
    }
}
