package com.sos.scheduler.engine.tunnel

import akka.util.ByteString
import scala.concurrent.Promise

/**
 * @author Joacim Zschimmer
 */
final case class DirectedRequest(tunnelIdWithPassword: TunnelId.WithPassword, request: Request)

object DirectedRequest {
  def apply(tunnelIdWithPassword: TunnelId.WithPassword, message: ByteString, responsePromise: Promise[ByteString]): DirectedRequest =
    DirectedRequest(tunnelIdWithPassword, Request(message, responsePromise))
}
