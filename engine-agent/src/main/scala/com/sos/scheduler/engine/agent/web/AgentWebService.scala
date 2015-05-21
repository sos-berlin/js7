package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.{FileOrderSourceContent, RequestFileOrderSourceContent}
import com.sos.scheduler.engine.agent.web.AgentWebService._
import com.sos.scheduler.engine.agent.web.marshal.XmlString
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichJavaStream
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import java.nio.file.Files.getLastModifiedTime
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import spray.http.StatusCodes.BadRequest
import spray.httpx.SprayJsonSupport._
import spray.routing.{HttpService, HttpServiceActor}

/**
 * @author Joacim Zschimmer
 */
private[agent] trait AgentWebService extends HttpService {

  protected def executeCommand(command: String): Future[xml.Elem]

  private[agent] def route =
    (decompressRequest() | compressResponseIfRequested(())) {
      pathPrefix("jobscheduler") {
        path("engine" / "command") {
          post {
            entity(as[XmlString]) { case XmlString(command) ⇒
              optionalHeaderValueByName("Remote-Address") {   // Access to Remote-Address requires Spray configuration spray.can.remote-address-header = on
                case None ⇒ complete(BadRequest, "Client's IP address is unknown")
                case Some(clientIPAddress) ⇒
                  val future = executeCommand(modifyLegacySchedulerCommand(command = command, clientIPAddress = clientIPAddress))
                  onSuccess(future) { response ⇒ complete(response) }
              }
            }
          }
        } ~
        pathPrefix("agent") {
          post {
            path("fileOrderSource" / "newFiles") {
              entity(as[RequestFileOrderSourceContent]) { request ⇒
                val future = Future {
                  getFileOrderSourceContent(request) match {
                    case o if o.files.nonEmpty ⇒ o
                    case _ ⇒
                      blocking {
                        Thread.sleep(request.durationMillis)  // Blocks a thread!!!
                      }
                      getFileOrderSourceContent(request)
                  }
                }
                onSuccess(future) { response ⇒ complete(response) }
              }
            }
          }
        }
      }
    }

  private def getFileOrderSourceContent(request: RequestFileOrderSourceContent): FileOrderSourceContent = {
    val regex = request.regex match {
      case "" ⇒ ".*".r
      case o ⇒ o.r
    }
    val entries = autoClosing(Files.list(Paths.get(request.directory))) { javaStream ⇒
      (javaStream.toIterator
        filter { f ⇒ regex.findFirstIn(f.getFileName.toString).isDefined && !request.knownFiles(f.toString) }
        flatMap toEntryOption
      ).toVector
    }
    FileOrderSourceContent(entries sortBy { _.lastModifiedTime })
  }
}

object AgentWebService {
  trait AsActor extends HttpServiceActor with AgentWebService {
    final def receive = runRoute(route)
  }

  private def modifyLegacySchedulerCommand(command: String, clientIPAddress: String): String =
    SafeXML.loadString(command) match {
      case elem if elem.label == "remote_scheduler.start_remote_task" ⇒
        elem.copy(attributes = elem.attributes.append(new xml.UnprefixedAttribute("ip_address", clientIPAddress, xml.Null))).toString()
      case elem if elem.label == "remote_scheduler.remote_task.close" ⇒
        command
      case label ⇒ sys.error(s"Unexpected XML command: $label")
    }

  private def toEntryOption(file: Path): Option[FileOrderSourceContent.Entry] =
    try Some(FileOrderSourceContent.Entry(file.toString, getLastModifiedTime(file).toMillis))
    catch { case _: NoSuchFileException ⇒ None }
}
