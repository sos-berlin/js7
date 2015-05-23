package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.web.AgentWebService._
import com.sos.scheduler.engine.agent.web.marshal.XmlString
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutor
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.http.StatusCodes.BadRequest
import spray.httpx.SprayJsonSupport._
import spray.routing.{HttpService, HttpServiceActor}

/**
 * @author Joacim Zschimmer
 */
private[agent] trait AgentWebService extends HttpService {

  protected def executeCommand(command: Command): Future[Response]

  private[agent] def route =
    (decompressRequest() | compressResponseIfRequested(())) {
      pathPrefix("jobscheduler") {
        path("engine" / "command") {
          post {
            entity(as[XmlString]) { case XmlString(commandXml) ⇒
              optionalHeaderValueByName("Remote-Address") {   // Access to Remote-Address requires Spray configuration spray.can.remote-address-header = on
                case None ⇒ complete(BadRequest, "Client's IP address is unknown")
                case Some(clientIPAddress) ⇒
                  val command = addIPAddressToLegacySchedulerCommand(commandXml = commandXml, clientIPAddress = clientIPAddress)
                  val future = CommandXmlExecutor.execute(command) { cmd ⇒ executeCommand(cmd) }
                  onSuccess(future) { response ⇒ complete(response) }
              }
            }
          }
        } ~
        pathPrefix("agent") {
          path("command") {
            post {
              entity(as[Command]) { command ⇒
                val future = executeCommand(command)
                onSuccess(future) { response ⇒ complete(response) }
              }
            }
          }
        }
      }
    }
}

object AgentWebService {

  trait AsActor extends HttpServiceActor with AgentWebService {
    final def receive = runRoute(route)
  }

  private def addIPAddressToLegacySchedulerCommand(commandXml: String, clientIPAddress: String): String =
    SafeXML.loadString(commandXml) match {
      case elem if elem.label == "remote_scheduler.start_remote_task" ⇒
        elem.copy(attributes = elem.attributes.append(new xml.UnprefixedAttribute("ip_address", clientIPAddress, xml.Null))).toString()
      case elem if elem.label == "remote_scheduler.remote_task.close" ⇒
        commandXml
      case label ⇒ sys.error(s"Unexpected XML command: $label")
    }
}
