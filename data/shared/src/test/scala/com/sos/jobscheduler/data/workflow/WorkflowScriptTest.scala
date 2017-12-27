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
    val A = WorkflowGraph.JobNode(NodeId("A"), agentJobPath)
    val B = WorkflowGraph.JobNode(NodeId("B"), agentJobPath)
    val C = WorkflowGraph.JobNode(NodeId("C"), agentJobPath)
    val D = WorkflowGraph.JobNode(NodeId("D"), agentJobPath)
    val END = WorkflowGraph.EndNode(NodeId("END"))

    val statements = List(
      WorkflowScript.Job(A.id, A.job) → true,
      WorkflowScript.Goto(B.id)       → true,
      WorkflowScript.Job(C.id, B.job) → true,
      WorkflowScript.Goto(D.id)       → true,
      WorkflowScript.OnError(D.id)    → false,  // reducible
      WorkflowScript.Goto(D.id)       → false,  // reducible
      WorkflowScript.Job(D.id, B.job) → true,
      WorkflowScript.Goto(END.id)     → false,  // reducible
      WorkflowScript.End(END.id)      → true,
      WorkflowScript.Job(B.id, B.job) → true,
      WorkflowScript.Goto(C.id)       → true)
    assert(WorkflowScript(statements map (_._1)).reduce == WorkflowScript(statements collect { case (s, true) ⇒ s }))
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
