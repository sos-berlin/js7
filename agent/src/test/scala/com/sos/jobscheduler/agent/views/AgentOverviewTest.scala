package com.sos.jobscheduler.agent.views

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.data.system.JavaInformation
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.Instant
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AgentOverviewTest extends FreeSpec {

  "JSON" in {
    testJson(
      AgentOverview(
        startedAt = Instant.parse("2015-06-01T12:00:00Z"),
        version = "TEST-VERSION",
        buildId = "BUILD-ID",
        isTerminating = false,
        system = SystemInformation(hostname = "TEST-HOSTNAME"),
        java = JavaInformation(
          version = "x.y.z",
          JavaInformation.Memory(maximum = 3, total = 2, free = 1),
          systemProperties = Map("test" → "TEST"))),
      json"""{
        "startedAt": "2015-06-01T12:00:00Z",
        "version": "TEST-VERSION",
        "buildId": "BUILD-ID",
        "isTerminating": false,
        "java": {
          "version": "x.y.z",
          "memory": {
            "maximum": 3,
            "total": 2,
            "free": 1
          },
          "systemProperties": {
            "test": "TEST"
          }
        },
        "system": {
          "hostname": "TEST-HOSTNAME",
          "mxBeans": {}
         }
      }""")
  }
}
