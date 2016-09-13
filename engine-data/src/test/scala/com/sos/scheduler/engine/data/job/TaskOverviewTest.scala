package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TaskOverviewTest extends FreeSpec {

  "JSON" in {
    check(
      TaskOverview(
        TaskId(111),
        JobPath("/JOB"),
        TaskState.running,
        Some(ProcessClassPath("/PROCESS-CLASS")),
        Some(AgentAddress("http://AGENT")),
        Set(TaskObstacle.ProcessClassUnavailable, TaskObstacle.Suspended)),  // (No real combination)
      """{
        "taskId": "111",
        "jobPath": "/JOB",
        "state": "running",
        "processClassPath": "/PROCESS-CLASS",
        "agent": "http://AGENT",
        "obstacles": [
          {
            "TYPE": "ProcessClassUnavailable"
          },
          {
            "TYPE": "Suspended"
          }
        ]
      }""")
  }

  private def check(q: TaskOverview, json: String) = {
    assert(q.toJson == json.parseJson)
    assert(json.parseJson.convertTo[TaskOverview] == q)
  }
}
