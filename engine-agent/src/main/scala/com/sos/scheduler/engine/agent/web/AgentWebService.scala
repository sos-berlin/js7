package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.FileOrderSourceContent
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichJavaStream
import java.net.InetAddress
import java.nio.file.{Files, NoSuchFileException, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
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
                    onSuccess(future) {
                      response ⇒ complete(response)
                    }
                }
            }
          }
        } ~
        pathPrefix("agent") {
          get {
            path("fileOrderSource" / "files") {
              parameter(('directory, 'order ! "latest-access-first")) { directoryString ⇒
                complete {
                  val directory = Paths.get(directoryString)
                  val entries = Files.list(directory).toVector flatMap { file ⇒
                    try Some(FileOrderSourceContent.Entry(file.toString, Files.getLastModifiedTime(file).toMillis))
                    catch { case _: NoSuchFileException ⇒ None }
                  }
                  FileOrderSourceContent(entries sortBy { _.lastModifiedTime })
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
}
