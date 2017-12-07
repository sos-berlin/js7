package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestWorkflow
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class AgentCommandTest extends FreeSpec {

  "Batch" in {
    check(AgentCommand.Batch(List(AgentCommand.NoOperation, AgentCommand.Logout)),
      """{
        "TYPE": "Batch",
        "commands": [
          { "TYPE": "NoOperation" },
          { "TYPE": "Logout" }
        ]
      }""")
  }

  "AbortImmediately" in {
    check(AgentCommand.AbortImmediately,
      """{ "TYPE": "AbortImmediately" }""")
  }

  "Login" in {
    check(AgentCommand.Login,
      """{
        "TYPE": "Login"
      }""")
  }

  "Logout" in {
    check(AgentCommand.Logout,
      """{
        "TYPE": "Logout"
      }""")
  }

  "NoOperation" in {
    check(AgentCommand.NoOperation,
      """{
        "TYPE": "NoOperation"
      }""")
  }

  "RegisterAsMaster" in {
    check(AgentCommand.RegisterAsMaster,
      """{
        "TYPE": "RegisterAsMaster"
      }""")
  }

  "Terminate" - {
    "JSON without sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true),
        """{
          "TYPE":"Terminate",
          "sigtermProcesses": true
        }""")
    }

    "JSON with sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(30.seconds)),
        """{
          "TYPE":"Terminate",
          "sigtermProcesses": true,
          "sigkillProcessesAfter": 30
        }""")
    }
  }

  "OrderCommand" - {
    "AttachOrder" in {
      check(AgentCommand.AttachOrder(
        Order(
          OrderId("ORDER-ID"),
          NodeKey(TestWorkflow.path, NodeId("INPUT")),
          Order.Ready,
          Some(Order.AttachedTo.Agent(AgentPath("/AGENT")))),
        AgentPath("/AGENT"),
        TestWorkflow),
        """{
          "TYPE": "AttachOrder",
          "order": {
            "id": "ORDER-ID",
            "state": {
              "TYPE": "Ready"
            },
            "nodeKey": {
              "nodeId": "INPUT",
              "workflowPath": "/WORKFLOW"
            },
            "attachedTo": {
              "agentPath": "/AGENT",
              "TYPE": "Agent"
            },
            "payload": {
              "outcome": {
                "returnValue": true,
                "TYPE": "Good"
              },
              "variables": {}
            }
          },
          "workflow": {
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
                    "agentPath": "/AGENT",
                    "jobPath": "/A"
                  }, {
                    "TYPE": "JobNode",
                    "id": "B",
                    "agentPath": "/AGENT",
                    "jobPath": "/B"
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
                    "agentPath": "/AGENT",
                    "jobPath": "/B"
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
          }
        }""")
    }

    "DetachOrder" in {
      check(AgentCommand.DetachOrder(OrderId("ORDER-ID")),
        """{
          "TYPE": "DetachOrder",
          "orderId": "ORDER-ID"
        }""")
    }

    "GetOrder" in {
      check(AgentCommand.GetOrder(OrderId("ORDER-ID")),
         """{
          "TYPE": "GetOrder",
          "orderId": "ORDER-ID"
        }""")
    }

    "GetOrderIds" in {
      check(AgentCommand.GetOrderIds,
        """{
          "TYPE": "GetOrderIds"
        }""")
    }
  }

  private def check(command: AgentCommand, jsonString: String): Unit =
    testJson(command, jsonString)
}
