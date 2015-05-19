package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.{FileOrderSourceContent, RequestFileOrderSourceContent}
import com.sos.scheduler.engine.agent.web.AgentWebService._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichJavaStream
import java.net.InetAddress
import java.nio.file.Files.getLastModifiedTime
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.http.StatusCodes.BadRequest
import spray.httpx.SprayJsonSupport._
import spray.routing.{HttpService, HttpServiceActor, RequestEntityExpectedRejection, UnsupportedRequestContentTypeRejection}

/**
 * @author Joacim Zschimmer
 */
private[agent] trait AgentWebService extends HttpService {

  protected def executeCommand(clientIPAddress: InetAddress, command: String): Future[xml.Elem]

  private[agent] def route =
    (decompressRequest() | compressResponseIfRequested(())) {
      pathPrefix("jobscheduler") {
        path("engine" / "command") {
          post {
            entity(as[HttpEntity]) {
              case HttpEntity.Empty ⇒ reject(RequestEntityExpectedRejection)
              case httpEntity: HttpEntity.NonEmpty ⇒
                if (!(Set(`application/xml`, `text/xml`) contains httpEntity.contentType.mediaType)) reject(UnsupportedRequestContentTypeRejection("application/xml expected"))
                optionalHeaderValueByName("Remote-Address") {
                  // Requires Spray configuration spray.can.remote-address-header = on
                  case None ⇒ complete(BadRequest, "Client's IP address is unknown")
                  case Some(clientIPAddress) ⇒
                    val future = executeCommand(clientIPAddress = InetAddress.getByName(clientIPAddress), command = httpEntity.asString)
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

  private def toEntryOption(file: Path): Option[FileOrderSourceContent.Entry] =
    try Some(FileOrderSourceContent.Entry(file.toString, getLastModifiedTime(file).toMillis))
    catch { case _: NoSuchFileException ⇒ None }
}
