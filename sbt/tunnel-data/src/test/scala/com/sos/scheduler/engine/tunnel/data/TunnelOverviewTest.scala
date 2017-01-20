package com.sos.scheduler.engine.tunnel.data

import java.net.InetAddress
import org.scalatest.FreeSpec
import spray.json.{pimpAny, pimpString}

/**
 * @author Joacim Zschimmer
 */
final class TunnelOverviewTest extends FreeSpec {

  "JSON" in {
    val obj = TunnelOverview(
      TunnelId("TUNNEL-1"),
      startedByHttpIp = Some(InetAddress.getByName("127.1.2.3")),
      remoteTcpAddress = Some("REMOTE-ADDRESS"))
    val json = """{
        "id": "TUNNEL-1",
        "startedByHttpIp": "127.1.2.3",
        "remoteTcpAddress": "REMOTE-ADDRESS"
      }""".parseJson
    assert(obj.toJson == json)
    assert(obj == json.convertTo[TunnelOverview])
  }
}
