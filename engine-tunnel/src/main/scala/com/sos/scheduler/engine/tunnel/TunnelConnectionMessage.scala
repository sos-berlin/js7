package com.sos.scheduler.engine.tunnel

import akka.util.ByteString
import com.sos.scheduler.engine.tunnel.data.TunnelToken
import spray.json.DefaultJsonProtocol.jsonFormat1
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final case class TunnelConnectionMessage(tunnelToken: TunnelToken) {

  /**
   * Binary representation for TCP connection.
   */
  def toByteString = TunnelConnectionMessage.toByteString(this)
}

object TunnelConnectionMessage {
  implicit val MyJsonFormat = jsonFormat1(apply)

  private def toByteString(o: TunnelConnectionMessage) = ByteString.fromString(o.toJson.toString())
}

