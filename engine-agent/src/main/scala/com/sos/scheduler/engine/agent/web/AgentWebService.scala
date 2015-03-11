package com.sos.scheduler.engine.agent.web

import java.net.InetAddress
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.http.StatusCodes.BadRequest
import spray.routing.{HttpService, HttpServiceActor, RequestEntityExpectedRejection, UnsupportedRequestContentTypeRejection}

/**
 * @author Joacim Zschimmer
 */
private[agent] trait AgentWebService extends HttpService {

  protected def executeCommand(clientIPAddress: InetAddress, command: String): Future[xml.Elem]

  private[agent] def route =
    (decompressRequest() | compressResponseIfRequested(())) {
      path("jobscheduler" / "engine" / "command") {
        post {
          entity(as[HttpEntity]) {
            case HttpEntity.Empty ⇒ reject(RequestEntityExpectedRejection)
            case httpEntity: HttpEntity.NonEmpty ⇒
              if (!(Set(`application/xml`, `text/xml`) contains httpEntity.contentType.mediaType)) reject(UnsupportedRequestContentTypeRejection("application/xml expected"))
              optionalHeaderValueByName("Remote-Address") {  // Requires Spray configuration spray.can.remote-address-header = on
                case None ⇒ complete(BadRequest, "Client's IP address is unknown")
                case Some(clientIPAddress) ⇒
                  val future = executeCommand(clientIPAddress = InetAddress.getByName(clientIPAddress), command = httpEntity.asString)
                  onSuccess(future) {
                    response ⇒ complete(response)
                  }
              }
          }
        }
      }
    }
}

object AgentWebService {
  trait AsActor extends HttpServiceActor with AgentWebService {
    def receive = runRoute(route)
  }
}
