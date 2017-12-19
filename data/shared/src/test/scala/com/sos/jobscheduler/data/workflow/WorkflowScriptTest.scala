package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowScriptTest extends FreeSpec {

  "JSON" in {
    testJson(ForkTestSetting.TestWorkflowScript, """{
      "statements": [
        { "TYPE": "Job", "nodeId": "A", "agentPath": "/AGENT-A", "jobPath": "/JOB" },
        {
          "TYPE": "ForkJoin",
          "idToRoute": {
            "🥕": {
              "statements": [
                { "TYPE": "Job", "nodeId": "Bx", "agentPath": "/AGENT-A", "jobPath": "/JOB" },
                { "TYPE": "Job", "nodeId": "Cx", "agentPath": "/AGENT-A", "jobPath": "/JOB" }
              ]
            },
            "🍋": {
              "statements": [
                { "TYPE": "Job", "nodeId": "By", "agentPath": "/AGENT-A", "jobPath": "/JOB" },
                { "TYPE": "Job", "nodeId": "Cy", "agentPath": "/AGENT-B", "jobPath": "/JOB" }
              ]
            }
          }
        },
        { "TYPE": "Job", "nodeId": "D", "agentPath": "/AGENT-A", "jobPath": "/JOB" },
        {
          "TYPE": "ForkJoin",
          "idToRoute": {
            "🥕": {
              "statements": [
                { "TYPE": "Job", "nodeId": "Ex", "agentPath": "/AGENT-A", "jobPath": "/JOB" },
                { "TYPE": "Job", "nodeId": "Fx", "agentPath": "/AGENT-A", "jobPath": "/JOB" }
              ]
            }, "🍋": {
              "statements": [
                { "TYPE": "Job", "nodeId": "Ey", "agentPath": "/AGENT-A", "jobPath": "/JOB" },
                { "TYPE": "Job", "nodeId": "Fy", "agentPath": "/AGENT-A", "jobPath": "/JOB" }
              ]
            }
          }
        },
        { "TYPE": "Job", "nodeId": "G", "agentPath": "/AGENT-A", "jobPath": "/JOB" },
        { "TYPE": "End", "nodeId": "END" }
      ]
    }""")
  }
}
