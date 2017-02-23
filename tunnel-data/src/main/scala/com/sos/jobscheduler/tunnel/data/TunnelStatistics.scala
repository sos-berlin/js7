package com.sos.jobscheduler.tunnel.data

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TunnelStatistics(
  requestCount: Int,
  messageByteCount: Long,
  currentRequestIssuedAt: Option[Instant],
  failure: Option[String])

object TunnelStatistics {
  implicit val MyJsonFormat = jsonFormat4(apply)
}
