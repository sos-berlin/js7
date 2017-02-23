package com.sos.scheduler.engine.tunnel.client

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
import com.sos.scheduler.engine.common.akkautils.Akkas
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.ByteStringMarshallers._
import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatRequestor
import com.sos.scheduler.engine.tunnel.client.WebTunnelClient._
import com.sos.scheduler.engine.tunnel.data.Http._
import com.sos.scheduler.engine.tunnel.data.TunnelToken
import scala.concurrent.{ExecutionContext, Future}
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.StatusCodes.OK
import spray.http.{HttpRequest, HttpResponse, Uri}
import spray.httpx.encoding.Gzip

/**
 * @author Joacim Zschimmer
 */
abstract class WebTunnelClient(
  tunnelToken: TunnelToken,
  val tunnelUri: Uri,
  heartbeatRequestorOption: Option[HeartbeatRequestor])
  (implicit actorSystem: ActorSystem)
extends AutoCloseable {

  protected def tunnelSendReceive(timeout: Timeout)(implicit executionContext: ExecutionContext): SendReceive

  private implicit def executionContext = actorSystem.dispatcher
  private val veryLongTimeout = Akkas.maximumTimeout(actorSystem.settings)

  private lazy val pipelineTrunk: HttpRequest ⇒ Future[HttpResponse] =
    addHeader(Accept(`application/octet-stream`)) ~>
    encode(Gzip) ~>
    tunnelSendReceive(veryLongTimeout) ~>
    decode(Gzip)

  final def close() = heartbeatRequestorOption foreach { _.close() }

  final def tunnelRequest(requestMessage: ByteString): Future[ByteString] = {
    val mySendReceive = addHeader(SecretHeaderName, tunnelToken.secret.string) ~> pipelineTrunk
    val request = Post(tunnelUri, requestMessage)
    try
      heartbeatRequestorOption match {
        case Some(requestor) ⇒ requestor.apply(mySendReceive, request) map {
          case o @ HttpResponse(OK, _, _, _) ⇒ o ~> unmarshal[ByteString]
          case HttpResponse(status, entity, _, _) ⇒ sys.error(s"Unexpected tunnel response: $status" + (if (status.isFailure) s": ${entity.asString take 500}" else ""))
        }
        case None ⇒ (mySendReceive ~> unmarshal[ByteString]).apply(request)
      }
    catch { case t: IllegalArgumentException ⇒
      logger.error(s"akka.scheduler.tick-duration is below 1s? $t")
      throw t
    }
  }

  override def toString = s"WebTunnelClient($tunnelUri)"
}

object WebTunnelClient {
  private val logger = Logger(getClass)
}
