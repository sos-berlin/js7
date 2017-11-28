package com.sos.jobscheduler.taskserver.data

import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TaskServerArgumentsTest extends FreeSpec {

  "logFilenamePart" in {
    assertResult("task-TEST-JOB-PATH-1-1") {
      TaskServerArguments.forTest().logFilenamePart
    }
  }

  "JSON" in {
    testJson(
      TaskServerArguments.forTest().copy(
        environment = Map("a" → "A", "bb" → "BB"),
        workingDirectory = Paths.get("DIRECTORY"),
        logDirectory = Paths.get("LOG-DIRECTORY")),
      """{
        "agentTaskId": "1-1",
        "stdFileMap": {},
        "logStdoutAndStderr": false,
        "environment": {
          "a": "A",
          "bb": "BB"
        },
        "jobPath": "/TEST-JOB-PATH",
        "logDirectory": "LOG-DIRECTORY",
        "workingDirectory": "DIRECTORY",
        "dotnet": {}
      }""")
  }
}
