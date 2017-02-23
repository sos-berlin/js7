package com.sos.scheduler.engine.tunnel.server

import akka.util.ByteString

/**
 * @author Joacim Zschimmer
 */
trait TunnelListener {
  def onRequest(message: ByteString): TunnelListener
}

object TunnelListener {
  object StopListening extends TunnelListener {
    def onRequest(message: ByteString) = this
  }
}
