package com.sos.scheduler.engine.tunnel.client

import akka.actor.ActorRefFactory
import akka.util.{ByteString, Timeout}
import com.sos.scheduler.engine.common.sprayutils.ByteStreamMarshallers._
import com.sos.scheduler.engine.tunnel.data.Http._
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.util.concurrent.TimeUnit.SECONDS
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

  protected implicit def actorRefFactory: ActorRefFactory
  protected def tunnelUri(tunnelId: TunnelId): Uri

  private implicit def executionContext = actorRefFactory.dispatcher
  private val neverTimeout = Timeout(Int.MaxValue - 2, SECONDS)  // 68 years, maximum for scheduler.tick-duration = 1s

  private lazy val pipelineTrunk: HttpRequest ⇒ Future[ByteString] =
    addHeader(Accept(`application/octet-stream`)) ~>
      encode(Gzip) ~>
      sendReceive(actorRefFactory, actorRefFactory.dispatcher, neverTimeout) ~>
      decode(Gzip) ~>
      unmarshal[ByteString]

  final def tunnelRequest(tunnelToken: TunnelToken, requestMessage: ByteString): Future[ByteString] = {
    val TunnelToken(id, secret) = tunnelToken
    val pipeline = addHeader(SecretHeaderName, secret.string) ~> pipelineTrunk
    pipeline(Post(tunnelUri(id), requestMessage))
  }
}
