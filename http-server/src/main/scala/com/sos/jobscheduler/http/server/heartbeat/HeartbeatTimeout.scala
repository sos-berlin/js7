package com.sos.jobscheduler.http.server.heartbeat

import com.sos.jobscheduler.http.client.heartbeat.{HeartbeatId, HttpHeartbeatTiming}
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
final case class HeartbeatTimeout(heartbeatId: HeartbeatId, since: Instant, timing: HttpHeartbeatTiming, name: String)
