package com.sos.scheduler.engine.agent.tunnel

import akka.actor.ActorRef
import akka.util.ByteString
import com.sos.scheduler.engine.agent.tunnel.Relais.DirectedRequest
import scala.concurrent.{Future, Promise}

/**
 * @author Joacim Zschimmer
 */
final class TunnelClient(relaisHandler: ActorRef, val tunnelId: TunnelId) {

  def sendRequest(message: ByteString): Future[ByteString] = {
    val responsePromise = Promise[ByteString]()
    relaisHandler ! DirectedRequest(tunnelId, message, responsePromise)
    responsePromise.future
  }

  def close(): Unit = {
    relaisHandler ! RelaisHandler.CloseTunnel(tunnelId)
  }
}
