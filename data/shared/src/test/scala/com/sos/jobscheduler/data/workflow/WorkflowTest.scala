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

    "start" in {
      assert(TestWorkflow.start == A.id)
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

    val a = Transition(A.id, B.id)
    val b = Transition(B.id, C.id)
    val cd = Transition(List(C.id, D.id), List(E.id), JoinTransition/*Dummy*/)
    val e = Transition(E.id, F.id)
    val f = Transition(F.id, G.id)
    val g = Transition(G.id, END.id)

    val workflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, END.id, List(A, B, C, D, E, F, G, END), List(a, b, cd, e, f, g))
    assert(workflow.reduceForAgent(u) == Workflow(workflow.path, A.id, END.id, List(A, B, C), List(a, b)))
    assert(workflow.reduceForAgent(v) == Workflow(workflow.path, A.id, END.id, List(D, E, F), List(e)))
    assert(workflow.reduceForAgent(w) == Workflow(workflow.path, A.id, END.id, List(G)      , List()))
  }

  "JSON" in {
    testJson(TestWorkflow,
      """{
        "path": "/WORKFLOW",
         "end": "END",
         "start": "A",
         "nodes": [
           {
             "TYPE": "JobNode",
             "id": "A",
             "agentPath": "/AGENT",
             "jobPath": "/A"
           }, {
             "TYPE": "JobNode",
             "id": "B",
             "agentPath": "/AGENT",
             "jobPath": "/B"
           }, {
             "TYPE": "EndNode",
             "id": "END"
           }
         ],
         "transitions": [
          {
            "outlets": [
              {
                "id": "B",
                "nodeId": "B"
              }
            ],
            "fromProcessedNodeIds": [ "A" ],
            "transitionType": {
              "TYPE": "ForwardTransition"
            }
          },
          {
            "outlets": [
              {
                "id": "END",
                "nodeId": "END"
              }
            ],
            "fromProcessedNodeIds": [ "B" ],
            "transitionType": {
              "TYPE": "ForwardTransition"
            }
          }
        ]
      }""")
  }
}
