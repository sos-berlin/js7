package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.Response
import com.sos.scheduler.engine.agent.web.LegacyCommandService._
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.agent.web.marshal.XmlString
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutor
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.http.StatusCodes.BadRequest
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait LegacyCommandService extends ServiceStandards {

  protected def executeCommand(command: Command): Future[Response]

  addRoute {
    (post & path("engine" / "command")) {
      optionalHeaderValueByName("Remote-Address") {
        // Access to Remote-Address requires Spray configuration spray.can.remote-address-header = on
        case None ⇒ complete(BadRequest, "Client's IP address is unknown")
        case Some(clientIPAddress) ⇒
          entity(as[XmlString]) { case XmlString(commandXml) ⇒
            val command = addIPAddressToLegacySchedulerCommand(commandXml = commandXml, clientIPAddress = clientIPAddress)
            val future = CommandXmlExecutor.execute(command) { cmd ⇒ executeCommand(cmd) }
            onSuccess(future) { response ⇒ complete(response) }
          }
      }
    }
  }
}

object LegacyCommandService {
  private def addIPAddressToLegacySchedulerCommand(commandXml: String, clientIPAddress: String): String = {
    val elem = SafeXML.loadString(commandXml)
    if (elem.label == StartProcess.XmlElementName)
      elem.copy(attributes = elem.attributes.append(new xml.UnprefixedAttribute("ip_address", clientIPAddress, xml.Null))).toString()
    else
      commandXml
  }
}
