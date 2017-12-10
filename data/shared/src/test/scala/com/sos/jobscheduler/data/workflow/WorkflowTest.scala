package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.JoinTransition
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends FreeSpec {

  "TestWorkflow" - {
    import ForkTestSetting._

    "inputNodeId" in {
      assert(TestWorkflow.inputNodeId == A.id)
    }

    "nodes" in {
      assert(TestWorkflow.nodes.toSet == Set(A, Bx, By, Cx, Cy, D, Ex, Ey, Fx, Fy, G, END))
    }

    "forkNodeToJoiningTransition" in {
      assert(TestWorkflow.forkNodeToJoiningTransition == Map(A.id → c, D.id → f))
    }

    //assert(TestWorkflow.nodeToInputTransition == Map(
    //  Bx.id → a,
    //  By.id → a,
    //  Cx.id → bx,
    //  Cy.id → by,
    //  D.id → c,
    //  Ex.id → d,
    //  Ey.id → d,
    //  Fx.id → ex,
    //  Fy.id → ey,
    //  G.id → f,
    //  END.id → g))

    "nodeToOutputTransition" in {
      assert(TestWorkflow.nodeToOutputTransition == Map(
        A.id → a,
        Bx.id → bx,
        By.id → by,
        Cx.id → c,
        Cy.id → c,
        D.id → d,
        Ex.id → ex,
        Ey.id → ey,
        Fx.id → f,
        Fy.id → f,
        G.id → g))
    }
  }

  "reduceForAgent" in {
    val u = AgentPath("/X")
    val v = AgentPath("/Y")
    val w = AgentPath("/Z")
    val jobPath = JobPath("/JOB")

    val A = JobNode(NodeId("A"), u, jobPath)
    val B = JobNode(NodeId("B"), u, jobPath)
    val C = JobNode(NodeId("C"), u, jobPath)
    val D = JobNode(NodeId("D"), v, jobPath)
    val E = JobNode(NodeId("E"), v, jobPath)
    val F = JobNode(NodeId("F"), v, jobPath)
    val G = JobNode(NodeId("G"), w, jobPath)
    val END = EndNode(NodeId("END"))

    val a = Transition(A, B)
    val b = Transition(B, C)
    val cd = Transition(List(C, D), List(E), JoinTransition/*Dummy*/)
    val e = Transition(E, F)
    val f = Transition(F, G)
    val g = Transition(G, END)

    val workflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, List(a, b, cd, e, f, g))
    assert(workflow.reduceForAgent(u) == Workflow(workflow.path, A.id, List(a, b)))
    assert(workflow.reduceForAgent(v) == Workflow(workflow.path, A.id, List(e), List(D)))
    assert(workflow.reduceForAgent(w) == Workflow(workflow.path, A.id, List(), List(G)))
  }

  "JSON" in {
    testJson(TestWorkflow,
      """{
        "path": "/WORKFLOW",
        "inputNodeId": "A",
        "transitions": [
          {
            "fromProcessedNodeIds": [ "A" ],
            "outlets": [
              {
                "id": "B",
                "nodeId": "B"
              }
            ],
            "nodes": [
              {
                "TYPE": "JobNode",
                "id": "A",
                "jobPath": "/A",
                "agentPath": "/AGENT"
              }, {
                "TYPE": "JobNode",
                "id": "B",
                "jobPath": "/B",
                "agentPath": "/AGENT"
              }
            ],
            "transitionType": {
              "TYPE": "ForwardTransition"
            }
          }, {
            "fromProcessedNodeIds": [ "B" ],
            "outlets": [
              {
                "id": "END",
                "nodeId": "END"
              }
            ],
             "nodes": [
               {
                "TYPE": "JobNode",
                "id": "B",
                "jobPath": "/B",
                "agentPath": "/AGENT"
              }, {
                "TYPE": "EndNode",
                "id": "END"
              }
            ],
            "transitionType": {
              "TYPE": "ForwardTransition"
            }
          }
        ],
        "unconnectedNodes": []
      }""")
  }
}
