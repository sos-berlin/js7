package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.Response
import com.sos.scheduler.engine.agent.web.AgentWebService._
import com.sos.scheduler.engine.agent.web.marshal.XmlString
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutor
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import java.nio.file.{Files, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.http.StatusCodes.{BadRequest, NotFound, OK}
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
            optionalHeaderValueByName("Remote-Address") {   // Access to Remote-Address requires Spray configuration spray.can.remote-address-header = on
              case None ⇒ complete(BadRequest, "Client's IP address is unknown")
              case Some(clientIPAddress) ⇒
                entity(as[XmlString]) { case XmlString(commandXml) ⇒
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
                onSuccess(future) { response: Response ⇒ complete(response) }
              }
            }
          } ~
          get {
            path("fileStatus") {
              parameter("file") { path ⇒
                complete {
                  val file = Paths.get(path)
                  if (Files.exists(file)) OK else NotFound
                }
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

  private def addIPAddressToLegacySchedulerCommand(commandXml: String, clientIPAddress: String): String = {
    val elem = SafeXML.loadString(commandXml)
    if (elem.label == StartProcess.XmlElementName)
      elem.copy(attributes = elem.attributes.append(new xml.UnprefixedAttribute("ip_address", clientIPAddress, xml.Null))).toString()
    else
      commandXml
  }
}
