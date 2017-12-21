package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowScriptTest extends FreeSpec {

  "reduce" in {
    val agentJobPath = AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-A"))
    val A = Workflow.JobNode(NodeId("A"), agentJobPath)
    val B = Workflow.JobNode(NodeId("B"), agentJobPath)
    val C = Workflow.JobNode(NodeId("C"), agentJobPath)
    val D = Workflow.JobNode(NodeId("D"), agentJobPath)
    val END = Workflow.EndNode(NodeId("END"))

    val script = WorkflowScript(List(
        WorkflowScript.Job(A.id, A.job),
        WorkflowScript.Goto(B.id),
        WorkflowScript.Job(C.id, B.job),
        WorkflowScript.Goto(D.id),
        WorkflowScript.OnError(D.id),     // <-- reduce deletes this OnError
        WorkflowScript.Goto(D.id),        // <-- reduce deletes this Goto
        WorkflowScript.Job(D.id, B.job),
        WorkflowScript.Goto(END.id),      // <-- reduce deletes this Goto
        WorkflowScript.End(END.id),
        WorkflowScript.Job(B.id, B.job),
        WorkflowScript.Goto(C.id)))
    val expected = WorkflowScript(List(
        WorkflowScript.Job(A.id, A.job),
        WorkflowScript.Goto(B.id),
        WorkflowScript.Job(C.id, B.job),
        WorkflowScript.Goto(D.id),
        WorkflowScript.Job(D.id, B.job),
        WorkflowScript.End(END.id),
        WorkflowScript.Job(B.id, B.job),
        WorkflowScript.Goto(C.id)))
    assert(script.reduce == expected)
  }

  "JSON" in {
    testJson(ForkTestSetting.TestWorkflowScript, """{
      "statements": [
        { "TYPE": "Job", "nodeId": "A", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToGraph": {
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
          "idToGraph": {
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
