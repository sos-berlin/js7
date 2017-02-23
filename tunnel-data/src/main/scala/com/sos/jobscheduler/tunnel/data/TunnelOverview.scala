package com.sos.jobscheduler.tunnel.data

import com.sos.jobscheduler.base.sprayjson.InetAddressJsonSupport._
import java.net.InetAddress
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TunnelOverview(
  id: TunnelId,
  startedByHttpIp: Option[InetAddress],
  remoteTcpAddress: Option[String])

object TunnelOverview {
  implicit val MyJsonFormat = jsonFormat3(apply)
}
