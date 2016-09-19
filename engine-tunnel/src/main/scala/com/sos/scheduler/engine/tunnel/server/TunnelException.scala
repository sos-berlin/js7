package com.sos.scheduler.engine.tunnel.server

import com.sos.scheduler.engine.tunnel.data.TunnelId

/**
  * @author Joacim Zschimmer
  */
sealed trait TunnelException

final class TunnelConnectionClosedException(message: String) extends RuntimeException(message)

final class NoSuchTunnelException(tunnelId: TunnelId) extends NoSuchElementException(s"Unknown $tunnelId")
