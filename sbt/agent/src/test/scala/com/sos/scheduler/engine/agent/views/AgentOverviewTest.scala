package com.sos.scheduler.engine.agent.views

import com.sos.scheduler.engine.base.system.SystemInformation
import com.sos.scheduler.engine.data.system.JavaInformation
import java.time.Instant
import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class AgentOverviewTest extends FreeSpec {

  "JSON" in {
    val obj = AgentOverview(
      startedAt = Instant.parse("2015-06-01T12:00:00Z"),
      version = "TEST-VERSION",
      currentTaskCount = 111,
      totalTaskCount = 222,
      isTerminating = false,
      system = SystemInformation(hostname = "TEST-HOSTNAME"),
      java = JavaInformation(
        systemProperties = Map("test" → "TEST"),
        JavaInformation.Memory(maximum = 3, total = 2, free = 1)))
    val json =
      s"""{
        "startedAt": "2015-06-01T12:00:00Z",
        "version": "TEST-VERSION",
        "currentTaskCount": 111,
        "totalTaskCount": 222,
        "isTerminating": false,
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
    assert(obj.toJson == json)
    //assert(obj = json.convertTo[AgentOverview])
  }
}
