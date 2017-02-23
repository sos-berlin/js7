package com.sos.jobscheduler.tunnel.data

import com.sos.jobscheduler.base.sprayjson.InetAddressJsonSupport._
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.http.server.heartbeat.HeartbeatView
import java.net.InetAddress
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class TunnelView(
  id: TunnelId,
  startedAt: Instant,
  startedByHttpIp: Option[InetAddress],
  remoteTcpAddress: Option[String],
  heartbeat: HeartbeatView,
  statistics: TunnelStatistics)

object TunnelView {
  implicit val MyJsonFormat = jsonFormat6(apply)
}
