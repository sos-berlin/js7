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
        { "TYPE": "Job", "nodeId": "A", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToRoute": {
            "🥕": {
              "statements": [
                { "TYPE": "Job", "nodeId": "Bx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                { "TYPE": "Job", "nodeId": "Cx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
              ]
            },
            "🍋": {
              "statements": [
                { "TYPE": "Job", "nodeId": "By", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                { "TYPE": "Job", "nodeId": "Cy", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }}
              ]
            }
          }
        },
        { "TYPE": "Job", "nodeId": "D", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToRoute": {
            "🥕": {
              "statements": [
                { "TYPE": "Job", "nodeId": "Ex", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                { "TYPE": "Job", "nodeId": "Fx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
              ]
            },
            "🍋": {
              "statements": [
                { "TYPE": "Job", "nodeId": "Ey", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                { "TYPE": "Job", "nodeId": "Fy", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
              ]
            }
          }
        },
        { "TYPE": "Job", "nodeId": "G", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        { "TYPE": "End", "nodeId": "END" }
      ]
    }""")
  }
}
