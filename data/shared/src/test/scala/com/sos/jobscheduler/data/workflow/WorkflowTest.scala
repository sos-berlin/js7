package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import org.scalatest.FreeSpec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.sos.jobscheduler.data.workflow.transition.Transition

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends FreeSpec {

  "reduceForAgent" in {
    val x = AgentPath("/X")
    val y = AgentPath("/Y")
    val z = AgentPath("/Z")
    val jobPath = JobPath("/JOB")

    val A = JobNode(NodeId("A"), x, jobPath)
    val B = JobNode(NodeId("B"), x, jobPath)
    val C = JobNode(NodeId("C"), x, jobPath)
    val D = JobNode(NodeId("D"), y, jobPath)
    val E = JobNode(NodeId("E"), y, jobPath)
    val F = JobNode(NodeId("F"), z, jobPath)
    val END = EndNode(NodeId("END"))

    val a = Transition(A, B)
    val b = Transition(B, C)
    val c = Transition(C, D)
    val d = Transition(D, E)
    val e = Transition(E, F)
    val f = Transition(F, END)

    val workflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, List(a, b, c, d, e, f))
    assert(workflow.reduceForAgent(x) == Workflow(workflow.path, A.id, List(a, b)))
    assert(workflow.reduceForAgent(y) == Workflow(workflow.path, A.id, List(d), List()))
    assert(workflow.reduceForAgent(z) == Workflow(workflow.path, A.id, List(), List(F)))
  }

  "JSON" in {
    testJson(TestWorkflow,
      """{
        "path": "/WORKFLOW",
        "inputNodeId": "A",
        "transitions": [{
            "fromNodeIds": [ "A" ],
            "toNodeIds": [ "B" ],
            "nodes": [{
                "TYPE": "JobNode",
                "id": "A",
                "jobPath": "/A",
                "agentPath": "/AGENT"
              }, {
                "TYPE": "JobNode",
                "id": "B",
                "jobPath": "/B",
                "agentPath": "/AGENT"
              }],
            "transitionType": {
              "TYPE": "ForwardTransition"
            }
          }, {
            "fromNodeIds": [ "B" ],
            "toNodeIds": [ "END" ],
            "nodes": [ {
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
