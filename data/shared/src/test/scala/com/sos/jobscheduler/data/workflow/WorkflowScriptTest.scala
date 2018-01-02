package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, FlatStatement, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
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
      WorkflowScript.IfError(D.id)    → false,  // reducible
      WorkflowScript.Goto(D.id)       → false,  // reducible
      WorkflowScript.Job(D.id, B.job) → true,
      WorkflowScript.Goto(END.id)     → false,  // reducible
      WorkflowScript.End(END.id)      → true,
      WorkflowScript.Job(B.id, B.job) → true,
      WorkflowScript.Goto(C.id)       → true)
    assert(WorkflowScript(statements map (_._1)).reduce == WorkflowScript(statements collect { case (s, true) ⇒ s }))
  }

  "flatten" in {
    assert(ForkTestSetting.TestWorkflowScript.flatten == Vector(
      FlatStatement.Node(FlatStatement.Nesting.empty, Job(A.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🥕"), Job(Bx.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🥕"), Job(Cx.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🍋"), Job(By.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🍋"), Job(Cy.id, BAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting.empty, Job(D.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🥕"), Job(Ex.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🥕"), Job(Fx.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🍋"), Job(Ey.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🍋"), Job(Fy.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting.empty, Job(G.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🥕"), Job(Hx.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🥕"), Job(Ix.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🍋"), Job(Hy.id, BAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting("🍋"), Job(Iy.id, BAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting.empty, Job(J.id, AAgentJobPath)),
      FlatStatement.Node(FlatStatement.Nesting.empty, End(END.id))))
  }

  "JSON" in {
    testJson(ForkTestSetting.TestWorkflowScript, """{
      "source": "\"A\": job /JOB on /AGENT-A;\nfork(\n  \"🥕\" { \"Bx\": job /JOB on /AGENT-A; \"Cx\": job /JOB on /AGENT-A; },\n  \"🍋\" { \"By\": job /JOB on /AGENT-A; \"Cy\": job /JOB on /AGENT-B; });\n\"D\": job /JOB on /AGENT-A;\nfork(\n  \"🥕\" { \"Ex\": job /JOB on /AGENT-A; \"Fx\": job /JOB on /AGENT-A; },\n  \"🍋\" { \"Ey\": job /JOB on /AGENT-A; \"Fy\": job /JOB on /AGENT-A; });\n\"G\": job /JOB on /AGENT-A;\nfork(\n  \"🥕\" { \"Hx\": job /JOB on /AGENT-A; \"Ix\": job /JOB on /AGENT-A; },\n  \"🍋\" { \"Hy\": job /JOB on /AGENT-B; \"Iy\": job /JOB on /AGENT-B; });\n\"J\": job /JOB on /AGENT-A;\n\"END\": end;",
      "statements": [
        { "TYPE": "Job", "nodeId": "A", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToScript": [
            {
              "id": "🥕",
              "script": {
                "statements": [
                  { "TYPE": "Job", "nodeId": "Bx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "nodeId": "Cx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                ]
              }
            }, {
              "id": "🍋",
              "script": {
                "statements": [
                  { "TYPE": "Job", "nodeId": "By", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "nodeId": "Cy", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }}
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "nodeId": "D", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToScript": [
            {
              "id": "🥕",
              "script": {
                "statements": [
                  { "TYPE": "Job", "nodeId": "Ex", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "nodeId": "Fx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                ]
              }
            }, {
              "id": "🍋",
              "script": {
                "statements": [
                  { "TYPE": "Job", "nodeId": "Ey", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "nodeId": "Fy", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "nodeId": "G", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToScript": [
            {
              "id": "🥕",
              "script": {
                "statements": [
                  { "TYPE": "Job", "nodeId": "Hx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "nodeId": "Ix", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                ]
              }
            }, {
              "id": "🍋",
              "script": {
                "statements": [
                  { "TYPE": "Job", "nodeId": "Hy", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "nodeId": "Iy", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }}
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "nodeId": "J", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        { "TYPE": "End", "nodeId": "END" }
      ]
    }""")
  }
}
