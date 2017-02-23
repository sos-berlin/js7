package com.sos.scheduler.engine.tunnel.data

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class TunnelHandlerOverview(
  tcpAddress: Option[String],
  tunnelCount: Int) {
}

object TunnelHandlerOverview {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
