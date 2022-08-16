package js7.agent.data.views

import js7.base.circeutils.CirceUtils.*
import js7.base.system.SystemInformation
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.system.JavaInformation
import js7.tester.CirceJsonTester.testJson

/**
 * @author Joacim Zschimmer
 */
final class AgentOverviewTest extends OurTestSuite {

  "JSON" in {
    testJson(
      AgentOverview(
        startedAt = Timestamp.parse("2015-06-01T12:00:00Z"),
        version = "TEST-VERSION",
        buildId = "BUILD-ID",
        isTerminating = false,
        system = SystemInformation(hostname = "TEST-HOSTNAME"),
        java = JavaInformation(
          version = "x.y.z",
          availableProcessors = 8,
          JavaInformation.Memory(maximum = 3, total = 2, free = 1),
          systemProperties = Map("test" -> "TEST"))),
      json"""{
        "startedAt": 1433160000000,
        "version": "TEST-VERSION",
        "buildId": "BUILD-ID",
        "isTerminating": false,
        "java": {
          "version": "x.y.z",
          "availableProcessors": 8,
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
