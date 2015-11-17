package com.sos.scheduler.engine.tunnel.client

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.common.akkautils.Akkas
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.ByteStringMarshallers._
import com.sos.scheduler.engine.tunnel.client.WebTunnelClient._
import com.sos.scheduler.engine.tunnel.data.Http._
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import scala.concurrent.Future
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpRequest, Uri}
import spray.httpx.encoding.Gzip

/**
 * @author Joacim Zschimmer
 */
trait WebTunnelClient {

  def uri: Uri
  def tunnelUri(tunnelId: TunnelId): Uri
  protected implicit def actorSystem: ActorSystem

  private implicit def executionContext = actorSystem.dispatcher
  private val veryLongTimeout = Akkas.maximumTimeout(actorSystem.settings)

  private lazy val pipelineTrunk: HttpRequest ⇒ Future[ByteString] =
    addHeader(Accept(`application/octet-stream`)) ~>
      encode(Gzip) ~>
      sendReceive(actorSystem, actorSystem.dispatcher, veryLongTimeout) ~>
      decode(Gzip) ~>
      unmarshal[ByteString]

  final def tunnelRequest(tunnelToken: TunnelToken, requestMessage: ByteString): Future[ByteString] = {
    val TunnelToken(id, secret) = tunnelToken
    val pipeline = addHeader(SecretHeaderName, secret.string) ~> pipelineTrunk
    try pipeline(Post(tunnelUri(id), requestMessage))
    catch { case t: IllegalArgumentException ⇒
      logger.error(s"akka.scheduler.tick-duration is below 1s? $t")
      throw t
    }
  }

  override def toString = s"WebTunnelClient($uri)"
}

object WebTunnelClient {
  private val logger = Logger(getClass)
}
