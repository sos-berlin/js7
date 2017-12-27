package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.WorkflowGraph._
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.JoinTransition
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowGraphTest extends FreeSpec {

  "linearPath" - {
    "no transition" in {
      assert(Nil.linearPath(TestWorkflow.graph.start) == Some(List(A.id)))
    }

    "single transition" in {
      assert(List(g).linearPath(G.id) == Some(List(G.id, END.id)))
    }

    "single fork/join" in {
      assert(List(a, c).linearPath(A.id) == Some(List(A.id, D.id)))
    }

    "loop" in {
      pending
    }

    "TestForkSetting" in {
      assert(TestWorkflow.graph.linearPath == Some(List(A.id, D.id, G.id, END.id)))
    }
  }

  "TestWorkflow" - {
    "start" in {
      assert(TestWorkflow.graph.start == A.id)
    }

    "nodes" in {
      assert(TestWorkflow.graph.nodes.toSet == Set(A, Bx, By, Cx, Cy, D, Ex, Ey, Fx, Fy, G, END))
    }

    "forkNodeToJoiningTransition" in {
      assert(TestWorkflow.graph.forkNodeToJoiningTransition == Map(A.id → c, D.id → f))
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
      assert(TestWorkflow.graph.nodeToOutputTransition == Map(
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

    val graph = WorkflowGraph(A.id, List(A, B, C, D, E, F, G, END), List(a, b, cd, e, f, g),
      originalScript = Some(WorkflowScript(WorkflowScript.Job(A.id, A.job) :: Nil)))  // Will be removed
    assert(graph.reduceForAgent(u) == WorkflowGraph(A.id, List(A, B, C), List(a, b), originalScript = None))
    assert(graph.reduceForAgent(v) == WorkflowGraph(A.id, List(D, E, F), List(e)   , originalScript = None))
    assert(graph.reduceForAgent(w) == WorkflowGraph(A.id, List(G      ), List()    , originalScript = None))
  }

  "JSON" in {
    testJson(ForkTestSetting.TestWorkflow.graph,
      """{
        "start": "A",
        "transitions": [
          {
            "fromProcessedNodeIds": [ "A" ],
            "toNodeIds": [ "Bx", "By" ],
            "idToGraph": [
              {
                "id": "🥕",
                "graph": {
                  "start": "Bx",
                  "transitions": [
                    {
                      "idToGraph": [],
                      "fromProcessedNodeIds": [ "Bx" ],
                      "toNodeIds": [ "Cx" ],
                      "transitionType": {
                        "TYPE": "ForwardTransition"
                      }
                    }
                  ],
                  "nodes": [
                    { "TYPE": "JobNode", "id": "Bx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
                    { "TYPE": "JobNode", "id": "Cx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }}
                  ],
                  "originalScript": {
                    "statements": [
                      { "TYPE": "Job", "nodeId": "Bx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                      { "TYPE": "Job", "nodeId": "Cx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                    ]
                  }
                }
              }, {
                "id": "🍋",
                "graph": {
                  "start": "By",
                  "transitions": [
                    {
                      "idToGraph": [],
                      "fromProcessedNodeIds": [ "By" ],
                      "toNodeIds": [ "Cy" ],
                      "transitionType": {
                        "TYPE": "ForwardTransition"
                      }
                    }
                  ],
                  "nodes": [
                    { "TYPE": "JobNode", "id": "By", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
                    { "TYPE": "JobNode", "id": "Cy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-B" }}
                  ],
                  "originalScript": {
                    "statements": [
                      { "TYPE": "Job", "nodeId": "By", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                      { "TYPE": "Job", "nodeId": "Cy", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }}
                    ]
                  }
                }
              }
            ],
            "transitionType": {
              "TYPE": "ForkTransition"
            }
          }, {
            "fromProcessedNodeIds": [ "Cx", "Cy" ],
            "toNodeIds": [ "D" ],
            "idToGraph": [],
            "transitionType": {
              "TYPE": "JoinTransition"
            },
            "forkNodeId": "A"
          }, {
            "fromProcessedNodeIds": [ "D" ],
            "toNodeIds": [ "Ex", "Ey" ],
            "idToGraph": [
              {
                "id": "🥕",
                "graph": {
                  "start": "Ex",
                  "transitions": [
                    {
                      "fromProcessedNodeIds": [ "Ex" ],
                      "toNodeIds": [ "Fx" ],
                      "idToGraph": [],
                      "transitionType": {
                        "TYPE": "ForwardTransition"
                      }
                    }
                  ],
                  "nodes": [
                    { "TYPE": "JobNode", "id": "Ex", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
                    { "TYPE": "JobNode", "id": "Fx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }}
                  ],
                  "originalScript": {
                    "statements": [
                      { "TYPE": "Job", "nodeId": "Ex", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                      { "TYPE": "Job", "nodeId": "Fx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                    ]
                  }
                }
              }, {
                "id": "🍋",
                "graph": {
                  "start": "Ey",
                  "transitions": [
                    {
                      "fromProcessedNodeIds": [ "Ey" ],
                      "toNodeIds": [ "Fy" ],
                      "idToGraph": [],
                      "transitionType": {
                        "TYPE": "ForwardTransition"
                      }
                    }
                  ],
                  "nodes": [
                    { "TYPE": "JobNode", "id": "Ey", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
                    { "TYPE": "JobNode", "id": "Fy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }}
                  ],
                  "originalScript": {
                    "statements": [
                      { "TYPE": "Job", "nodeId": "Ey", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                      { "TYPE": "Job", "nodeId": "Fy", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                    ]
                  }
                }
              }
            ],
            "transitionType": {
              "TYPE": "ForkTransition"
            }
          }, {
            "fromProcessedNodeIds": [ "Fx", "Fy" ],
            "toNodeIds": [ "G" ],
            "idToGraph": [],
            "transitionType": {
              "TYPE": "JoinTransition"
            },
            "forkNodeId": "D"
          }, {
            "fromProcessedNodeIds": [ "G" ],
            "toNodeIds": [ "END" ],
            "idToGraph": [],
            "transitionType": {
              "TYPE": "ForwardTransition"
            }
          }
        ],
        "nodes": [
          { "TYPE": "JobNode", "id": "A", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Bx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Cx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "By", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Cy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-B" }},
          { "TYPE": "JobNode", "id": "D", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Ex", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Fx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Ey", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Fy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "G", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "EndNode", "id": "END" }
        ],

        "originalScript": {
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
                  "id" : "🍋",
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
            { "TYPE": "End", "nodeId": "END" }
          ]
        }
      }""")
  }
}
