package com.sos.scheduler.engine.http.client.heartbeat

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatRequestHeaders._
import org.scalatest.FreeSpec
import spray.http.HttpHeaders.RawHeader
import spray.http.StatusCodes.{BadRequest, InternalServerError}
import spray.httpx.marshalling.BasicMarshallers._
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class HeartbeatRequestHeadersTest extends FreeSpec with ScalatestRouteTest {

  "X-JobScheduler-Heartbeat-Start" in {
    val name = "X-JobScheduler-Heartbeat-Start"
    val value = "PT12.3S PT30S"
    val headerLine = s"$name: $value"
    val header = `X-JobScheduler-Heartbeat-Start`(HttpHeartbeatTiming(period = 12300.ms, timeout = 30.s))
    assert(header.toString == headerLine)

    val `X-JobScheduler-Heartbeat-Start`.Value(times) = value
    assert(times == HttpHeartbeatTiming(period = 12300.ms, timeout = 30.s))
  }

  "X-JobScheduler-Heartbeat-Start, bad syntax" in {
    val route = headerValueByName(`X-JobScheduler-Heartbeat-Start`.name) { case `X-JobScheduler-Heartbeat-Start`.Value(timing) ⇒ complete("OKAY") }
    Post("/").withHeaders(RawHeader("X-JobScheduler-Heartbeat-Start", "PT1S PT1S")) ~> route ~> check {
      pendingUntilFixed {
        assert(status == BadRequest)
        assert(entity.asString contains "Bad header X-JobScheduler-Heartbeat-Start")
      }
      assert(status == InternalServerError)
    }
  }

  "X-JobScheduler-Heartbeat-Continue" in {
    val name = "X-JobScheduler-Heartbeat-Continue"
    val heartbeatId = HeartbeatId.generate()
    val value = s"${heartbeatId.string} PT10S PT30S"
    val headerLine = s"$name: $value"
    val header = `X-JobScheduler-Heartbeat-Continue`(heartbeatId, HttpHeartbeatTiming(period = 10.s, timeout = 30.s))
    assert(header.toString == headerLine)

    val `X-JobScheduler-Heartbeat-Continue`.Value(heartbeatId_, times) = value
    assert(heartbeatId_ == heartbeatId)
    assert(times == HttpHeartbeatTiming(period = 10.s, timeout = 30.s))
  }
}
