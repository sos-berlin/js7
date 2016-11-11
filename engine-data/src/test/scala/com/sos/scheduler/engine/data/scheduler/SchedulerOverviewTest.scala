package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.base.system.SystemInformation
import com.sos.scheduler.engine.data.system.JavaInformation
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

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
      httpPort = Some("127.0.0.1:4444"),
      httpsPort = Some("0.0.0.0:5555"),
      udpPort = Some(6666),
      supervisor = Some(SupervisorUri("tcp://example.com:4000")),
      pid = 77,
      SchedulerState.running,
      system = SystemInformation(hostname = "TEST-HOSTNAME"),
      java = JavaInformation(
        systemProperties = Map("test" → "TEST"),
        JavaInformation.Memory(maximum = 3, total = 2, free = 1)))
    val json = """{
      "version": "0.0",
      "startedAt": "2016-07-13T01:02:03.004Z",
      "schedulerId": "SCHEDULER-ID",
      "httpPort": "127.0.0.1:4444",
      "httpsPort": "0.0.0.0:5555",
      "udpPort": 6666,
      "supervisor": "tcp://example.com:4000",
      "pid": 77,
      "state": "running",
      "java": {
        "systemProperties": {
          "test": "TEST"
        },
        "memory": {
          "maximum": 3,
          "total": 2,
          "free": 1
        }
      },
      "system": {
        "hostname": "TEST-HOSTNAME",
        "mxBeans": {}
       }
    }""".parseJson
    assert(overview.toJson == json)
    assert(overview == json.convertTo[SchedulerOverview])
  }
}
