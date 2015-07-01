package com.sos.scheduler.engine.tunnel

import akka.util.ByteString
import com.sos.scheduler.engine.tunnel.data.TunnelId
import scala.concurrent.Promise

/**
 * @author Joacim Zschimmer
 */
final case class DirectedRequest private[tunnel](tunnelIdWithPassword: TunnelId.WithPassword, request: Relais.Request)

object DirectedRequest {
  def apply(tunnelIdWithPassword: TunnelId.WithPassword, message: ByteString, responsePromise: Promise[ByteString]): DirectedRequest =
    DirectedRequest(tunnelIdWithPassword, Relais.Request(message, responsePromise))
}
