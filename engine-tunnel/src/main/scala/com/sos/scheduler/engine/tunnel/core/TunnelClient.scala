package com.sos.scheduler.engine.tunnel.core

import akka.actor.ActorRef
import akka.util.ByteString
import com.sos.scheduler.engine.tunnel.data.TunnelToken
import java.net.InetSocketAddress
import org.jetbrains.annotations.TestOnly
import scala.concurrent.{Future, Promise}

/**
 * @author Joacim Zschimmer
 */
final class TunnelClient(
  connectorHandler: ActorRef,
  val tunnelToken: TunnelToken,
  val connected: Future[InetSocketAddress],
  peerAddress: () ⇒ Option[InetSocketAddress])
extends AutoCloseable {

  def id = tunnelToken.id

  @TestOnly
  private[tunnel] def sendRequest(message: ByteString): Future[ByteString] = {
    val responsePromise = Promise[ByteString]()
    connectorHandler ! ConnectorHandler.DirectedRequest(tunnelToken, message, responsePromise)
    responsePromise.future
  }

  def close(): Unit = connectorHandler ! ConnectorHandler.CloseTunnel(tunnelToken)

  override def toString = s"TunnelClient($id,${peerAddress() getOrElse "not yet connected"})"
}
