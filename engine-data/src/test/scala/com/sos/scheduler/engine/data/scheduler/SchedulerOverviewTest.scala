package com.sos.scheduler.engine.data.scheduler

import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.{pimpAny, pimpString}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SchedulerOverviewTest extends FreeSpec {

  "JSON" in {
    val overview = SchedulerOverview(
      version = "0.0",
      startedAt = Instant.parse("2016-07-13T01:02:03.004Z"),
      SchedulerId("SCHEDULER-ID"),
      httpPort = Some(4444),
      udpPort = Some(5555),
      pid = 77,
      SchedulerState.running)
    val json = """{
      "version": "0.0",
      "startedAt": "2016-07-13T01:02:03.004Z",
      "schedulerId": "SCHEDULER-ID",
      "httpPort": 4444,
      "udpPort": 5555,
      "pid": 77,
      "state": "running"
    }""".parseJson
    assert(overview.toJson == json)
    assert(overview == json.convertTo[SchedulerOverview])
  }
}
