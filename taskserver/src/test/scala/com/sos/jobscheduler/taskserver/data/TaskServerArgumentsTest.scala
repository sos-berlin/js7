package com.sos.scheduler.engine.taskserver.data

import com.sos.scheduler.engine.agent.data.commands.StartTask
import com.sos.scheduler.engine.data.job.TaskId
import java.nio.file.Paths
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class TaskServerArgumentsTest extends FreeSpec {

  "logFilenamePart" in {
    assertResult("task-1-1-test-123") {
      TaskServerArguments.forTest().copy(startMeta = StartTask.Meta(job = "/folder/test", TaskId(123))).logFilenamePart
    }
  }

  "logFilenamePart when master < v1.10.4" in {
    assertResult("task-1-1-(OLD-MASTER)--1") {
      TaskServerArguments.forTest().logFilenamePart
    }
  }

  "JSON" in {
    val obj = TaskServerArguments.forTest().copy(
      environment = Map("a" → "A", "bb" → "BB"),
      workingDirectory = Paths.get("DIRECTORY"),
      logDirectory = Paths.get("LOG-DIRECTORY"))
    val json = """{
        "agentTaskId": "1-1",
        "stdFileMap": {},
        "logStdoutAndStderr": false,
        "environment": {
          "a": "A",
          "bb": "BB"
        },
        "startMeta": {
          "job": "/(OLD-MASTER)",
          "taskId": "-1"
        },
        "tunnelToken": {
          "id": "TEST-TUNNEL",
          "secret": "TUNNEL-SECRET"
        },
        "masterAddress": "127.0.0.1:999999999",
        "logDirectory": "LOG-DIRECTORY",
        "workingDirectory": "DIRECTORY",
        "dotnet": {}
      }""".parseJson
    assert(obj.toJson == json)
    assert(obj == json.convertTo[TaskServerArguments])
  }
}
