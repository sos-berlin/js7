package com.sos.scheduler.engine.http.client.heartbeat

import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatResponseHeaders._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeader
import spray.http.HttpHeaders.`Cache-Control`
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class HeartbeatResponseHeadersTest extends FreeSpec with ScalatestRouteTest {

  "response.X-JobScheduler-Heartbeat" in {
    val name = "X-JobScheduler-Heartbeat"
    val heartbeatId = HeartbeatId.generate()
    val value = heartbeatId.string
    val headerLine = s"$name: $value"
    val header = `X-JobScheduler-Heartbeat`(heartbeatId)
    assert(header.toString == headerLine)

    val `X-JobScheduler-Heartbeat`.Value(heartbeatId_) = value
    assert(heartbeatId_ == heartbeatId)
    List[HttpHeader](`Cache-Control`(Nil), header) collect { case `X-JobScheduler-Heartbeat`(id) ⇒ id } shouldEqual List(heartbeatId)
  }
}
