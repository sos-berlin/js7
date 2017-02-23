package com.sos.scheduler.engine.tunnel.data

import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatId
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatView
import java.net.InetAddress
import java.time.Instant
import org.scalatest.FreeSpec
import spray.json.{pimpAny, pimpString}

/**
 * @author Joacim Zschimmer
 */
final class TunnelViewTest extends FreeSpec {

  "JSON" in {
    val obj = TunnelView(
      TunnelId("TUNNEL-1"),
      startedAt = Instant.parse("2015-12-08T12:00:00Z"),
      startedByHttpIp = Some(InetAddress.getByName("127.1.2.3")),
      remoteTcpAddress = Some("REMOTE-ADDRESS"),
      heartbeat = HeartbeatView(
        startCount = 3,
        count = 9,
        concurrentMaximum = 1,
        pendingOperations = Map(
          HeartbeatId("HEARTBEAT-1") → HeartbeatView.PendingOperation(
            startedAt = Instant.parse("2015-12-08T12:00:11Z"),
            lastAt = Some(Instant.parse("2015-12-08T12:00:22Z")),
            count = 7))),
      TunnelStatistics(
        requestCount = 10,
        messageByteCount = 1000,
        currentRequestIssuedAt = Some(Instant.parse("2015-07-03T12:00:00Z")),
        failure = Some("FAILURE")))
    val json = """{
        "id": "TUNNEL-1",
        "startedAt": "2015-12-08T12:00:00Z",
        "startedByHttpIp": "127.1.2.3",
        "remoteTcpAddress": "REMOTE-ADDRESS",
        "heartbeat": {
          "startCount": 3,
          "count": 9,
          "concurrentMaximum": 1,
          "pendingOperations": {
            "HEARTBEAT-1": {
              "startedAt": "2015-12-08T12:00:11Z",
              "lastAt": "2015-12-08T12:00:22Z",
              "count": 7
            }
          }
        },
        "statistics": {
          "requestCount": 10,
          "messageByteCount": 1000,
          "currentRequestIssuedAt": "2015-07-03T12:00:00Z",
          "failure": "FAILURE"
        }
      }""".parseJson
    assert(obj.toJson == json)
    assert(obj == json.convertTo[TunnelView])
  }
}
