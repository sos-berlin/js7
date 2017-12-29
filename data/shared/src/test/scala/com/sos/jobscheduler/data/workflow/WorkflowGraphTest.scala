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
      assert(List(g).linearPath(G.id) == Some(List(G.id)))
      assert(List(j).linearPath(J.id) == Some(List(J.id, END.id)))
    }

    "single fork/join" in {
      assert(List(a, c).linearPath(A.id) == Some(List(A.id, D.id)))
    }

    "fork branch" in {
      assert(List(ex).linearPath(Ex.id) == Some(List(Ex.id, Fx.id)))
    }

    "loop" in {
      pending
    }

    "TestForkSetting" in {
      assert(TestWorkflow.graph.linearPath == Some(List(A.id, D.id, G.id, J.id, END.id)))
    }
  }

  "TestWorkflow" - {
    "start" in {
      assert(TestWorkflow.graph.start == A.id)
    }

    "nodes" in {
      assert(TestWorkflow.graph.nodes.toSet == Set(A, Bx, By, Cy, Cx, D, Ex, Fx, Ey, Fy, G, Hx, Ix, Hy, Iy, J, END))
    }

    "forkNodeToJoiningTransition" in {
      assert(TestWorkflow.graph.forkNodeToJoiningTransition == Map(A.id → c, D.id → f, G.id → i))
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
        Cx.id → c,
        By.id → by,
        Cy.id → c,
        D.id → d,
        Ex.id → ex,
        Fx.id → f,
        Ey.id → ey,
        Fy.id → f,
        G.id → g,
        Hx.id → hx,
        Ix.id → i,
        Hy.id → hy,
        Iy.id → i,
        J.id → j))
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

  "reduceForAgent ForkTestSetting" in {
    import ForkTestSetting._
    assert(TestWorkflow.graph.reduceForAgent(AAgentPath) ==
      WorkflowGraph(A.id, Vector(A, Bx, Cx, By, D, Ex, Fx, Ey, Fy, G, Hx, Ix, J), Vector(a, d, f, hx), originalScript = None))
    assert(TestWorkflow.graph.reduceForAgent(AAgentPath).allTransitions == Vector(a, d, f, hx, bx, by, ex, ey))

    assert(TestWorkflow.graph.reduceForAgent(BAgentPath) ==
      WorkflowGraph(A.id, Vector(Cy, Hy, Iy), Vector(hy), originalScript = None))
    assert(TestWorkflow.graph.reduceForAgent(BAgentPath).allTransitions == Vector(hy))
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
            "toNodeIds": [ "Hx", "Hy" ],
            "idToGraph": [
              {
                "id": "🥕",
                "graph": {
                  "start": "Hx",
                  "transitions": [
                    {
                      "fromProcessedNodeIds": [ "Hx" ],
                      "toNodeIds": [ "Ix" ],
                      "idToGraph": [],
                      "transitionType": {
                        "TYPE": "ForwardTransition"
                      }
                    }
                  ],
                  "nodes": [
                    { "TYPE": "JobNode", "id": "Hx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
                    { "TYPE": "JobNode", "id": "Ix", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }}
                  ],
                  "originalScript": {
                    "statements": [
                      { "TYPE": "Job", "nodeId": "Hx", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                      { "TYPE": "Job", "nodeId": "Ix", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }}
                    ]
                  }
                }
              }, {
                "id": "🍋",
                "graph": {
                  "start": "Hy",
                  "transitions": [
                    {
                      "fromProcessedNodeIds": [ "Hy" ],
                      "toNodeIds": [ "Iy" ],
                      "idToGraph": [],
                      "transitionType": {
                        "TYPE": "ForwardTransition"
                      }
                    }
                  ],
                  "nodes": [
                    { "TYPE": "JobNode", "id": "Hy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-B" }},
                    { "TYPE": "JobNode", "id": "Iy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-B" }}
                  ],
                  "originalScript": {
                    "statements": [
                      { "TYPE": "Job", "nodeId": "Hy", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }},
                      { "TYPE": "Job", "nodeId": "Iy", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }}
                    ]
                  }
                }
              }
            ],
            "transitionType": {
              "TYPE": "ForkTransition"
            }
          }, {
            "fromProcessedNodeIds": [ "Ix", "Iy" ],
            "toNodeIds": [ "J" ],
            "idToGraph": [],
            "transitionType": {
              "TYPE": "JoinTransition"
            },
            "forkNodeId": "G"
          }, {
            "fromProcessedNodeIds": [ "J" ],
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
          { "TYPE": "JobNode", "id": "Hx", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Ix", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
          { "TYPE": "JobNode", "id": "Hy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-B" }},
          { "TYPE": "JobNode", "id": "Iy", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-B" }},
          { "TYPE": "JobNode", "id": "J", "job": { "jobPath": "/JOB", "agentPath": "/AGENT-A" }},
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
                  "id" : "🍋",
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
        }
      }""")
  }
}
