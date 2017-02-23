package com.sos.scheduler.engine.http.server.heartbeat

import com.sos.scheduler.engine.http.client.heartbeat.{HeartbeatId, HttpHeartbeatTiming}
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
final case class HeartbeatTimeout(heartbeatId: HeartbeatId, since: Instant, timing: HttpHeartbeatTiming, name: String)
