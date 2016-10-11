package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.filebased.FileBasedState
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ProcessClassDetailedTest extends FreeSpec {

  "JSON" in {
    check(
      ProcessClassDetailed(
        ProcessClassOverview(
          ProcessClassPath("/PROCESS-CLASS"),
          FileBasedState.active,
          processLimit = 10,
          usedProcessCount = 11,
          obstacles = Set(ProcessClassObstacle.ProcessLimitReached(10))),
        List(
          AgentAddress("https://example.com:4445"))),
      """{
        "overview": {
          "path": "/PROCESS-CLASS",
          "fileBasedState": "active",
          "processLimit": 10,
          "usedProcessCount": 11,
          "obstacles": [
            {
              "limit":10,
              "TYPE": "ProcessLimitReached"
            }
          ]
        },
      "agents": [
        "https://example.com:4445"
      ]
    }""")
  }

  private def check(q: ProcessClassDetailed, json: String) = {
    assert(q.toJson == json.parseJson)
    assert(json.parseJson.convertTo[ProcessClassDetailed] == q)
  }
}
