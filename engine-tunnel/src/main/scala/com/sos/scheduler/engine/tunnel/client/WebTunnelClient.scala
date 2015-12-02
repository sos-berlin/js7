package com.sos.scheduler.engine.tunnel.client

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.common.akkautils.Akkas
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.ByteStringMarshallers._
import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatRequestor
import com.sos.scheduler.engine.tunnel.client.WebTunnelClient._
import com.sos.scheduler.engine.tunnel.data.Http._
import com.sos.scheduler.engine.tunnel.data.TunnelToken
import scala.concurrent.Future
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpRequest, HttpResponse, Uri}
import spray.httpx.encoding.Gzip

/**
 * @author Joacim Zschimmer
 */
trait WebTunnelClient extends AutoCloseable {

  protected def tunnelToken: TunnelToken
  def tunnelUri: Uri
  protected val heartbeatRequestorOption: Option[HeartbeatRequestor]
  protected implicit def actorSystem: ActorSystem

  private implicit def executionContext = actorSystem.dispatcher
  private val veryLongTimeout = Akkas.maximumTimeout(actorSystem.settings)

  private lazy val pipelineTrunk: HttpRequest ⇒ Future[HttpResponse] =
    addHeader(Accept(`application/octet-stream`)) ~>
    encode(Gzip) ~>
    sendReceive(actorSystem, actorSystem.dispatcher, veryLongTimeout) ~>
    decode(Gzip)

  def close() = heartbeatRequestorOption foreach { _.close() }

  final def tunnelRequest(requestMessage: ByteString): Future[ByteString] = {
    val mySendReceive = addHeader(SecretHeaderName, tunnelToken.secret.string) ~> pipelineTrunk
    val request = Post(tunnelUri, requestMessage)
    try
      heartbeatRequestorOption match {
        case Some(requestor) ⇒ requestor.apply(mySendReceive, request) map { _ ~> unmarshal[ByteString] }
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
