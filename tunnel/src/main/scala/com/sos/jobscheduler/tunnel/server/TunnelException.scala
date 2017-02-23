package com.sos.jobscheduler.tunnel.server

import com.sos.jobscheduler.tunnel.data.TunnelId

/**
  * @author Joacim Zschimmer
  */
sealed trait TunnelException

final class TunnelConnectionClosedException(message: String) extends RuntimeException(message)

final class NoSuchTunnelException(tunnelId: TunnelId) extends NoSuchElementException(s"Unknown $tunnelId")
