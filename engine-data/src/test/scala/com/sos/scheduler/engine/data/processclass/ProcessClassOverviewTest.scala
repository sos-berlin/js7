package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.filebased.FileBasedState
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ProcessClassOverviewTest extends FreeSpec {

  "JSON" in {
    val o = ProcessClassOverview(
      ProcessClassPath("/PROCESS-CLASS"),
      FileBasedState.active,
      processLimit = 10,
      usedProcessCount = 11,
      obstacles = Set(ProcessClassObstacle.ProcessLimitReached(10)))
    val json = """{
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
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[ProcessClassOverview] == o)
  }
}
