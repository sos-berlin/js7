package com.sos.scheduler.engine.tunnel

import akka.actor.ActorRef
import akka.util.ByteString
import com.sos.scheduler.engine.tunnel.data.TunnelId
import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}

/**
 * @author Joacim Zschimmer
 */
final class TunnelClient(
  connectorHandler: ActorRef,
  val idWithPassword: TunnelId.WithPassword,
  val connected: Future[InetSocketAddress],
  peerAddress: () ⇒ Option[InetSocketAddress]) {

  def id = idWithPassword.id

  def sendRequest(message: ByteString): Future[ByteString] = {
    val responsePromise = Promise[ByteString]()
    connectorHandler ! ConnectorHandler.DirectedRequest(idWithPassword, message, responsePromise)
    responsePromise.future
  }

  def close(): Unit = connectorHandler ! ConnectorHandler.CloseTunnel(idWithPassword)

  override def toString = s"TunnelClient($id,${peerAddress() getOrElse "not yet connected"})"
}
