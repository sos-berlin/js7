package js7.agent.data.views

import js7.base.circeutils.CirceUtils._
import js7.base.system.SystemInformation
import js7.base.time.Timestamp
import js7.data.system.JavaInformation
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AgentOverviewTest extends AnyFreeSpec {

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
          JavaInformation.Memory(maximum = 3, total = 2, free = 1),
          systemProperties = Map("test" -> "TEST"))),
      json"""{
        "startedAt": 1433160000000,
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
