package com.sos.scheduler.engine.data.job

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TaskObstacleTest extends FreeSpec {
  import TaskObstacle._

  "LockUnavailable" in {
    check(LockUnavailable,
      """{
        "TYPE": "LockUnavailable"
      }""")
  }

  "ProcessClassUnavailable" in {
    check(ProcessClassUnavailable,
      """{
        "TYPE": "ProcessClassUnavailable"
      }""")
  }

  "AgentUnavailable" in {
    check(AgentUnavailable,
      """{
        "TYPE": "AgentUnavailable"
      }""")
  }

  "Suspended" in {
    check(Suspended,
      """{
        "TYPE": "Suspended"
      }""")
  }

  "Delayed" in {
    check(Delayed,
      """{
        "TYPE": "Delayed"
      }""")
  }

  private def check(q: TaskObstacle, json: String) = {
    assert(q.toJson == json.parseJson)
    assert(json.parseJson.convertTo[TaskObstacle] == q)
  }
}
