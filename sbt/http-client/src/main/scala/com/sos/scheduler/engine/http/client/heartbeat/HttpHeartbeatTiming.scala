package com.sos.scheduler.engine.http.client.heartbeat

import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Duration

/**
  * @author Joacim Zschimmer
  */
final case class HttpHeartbeatTiming(period: Duration, timeout: Duration) {
  require(period < timeout, "HTTP heartbeat period must be shorter than the timeout")

  override def toString = s"HttpHeartbeatTiming(timeout ${timeout.pretty}, beat ${period.pretty})"
}

object HttpHeartbeatTiming {
  val Default = HttpHeartbeatTiming(period = 10.s, timeout = 60.s)
}
