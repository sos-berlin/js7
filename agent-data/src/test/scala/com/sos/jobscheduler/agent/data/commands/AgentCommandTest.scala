package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.jobnet.Jobnet.{EndNode, JobNode}
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import org.scalatest.FreeSpec
import spray.json._

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
      }""".parseJson)
  }

  "AbortImmediately" in {
    check(AgentCommand.AbortImmediately,
      """{ "TYPE": "AbortImmediately" }""".parseJson)
  }

  "Login" in {
    check(AgentCommand.Login,
      """{
        "TYPE": "Login"
      }""".parseJson)
  }

  "Logout" in {
    check(AgentCommand.Logout,
      """{
        "TYPE": "Logout"
      }""".parseJson)
  }

  "NoOperation" in {
    check(AgentCommand.NoOperation,
      """{
        "TYPE": "NoOperation"
      }""".parseJson)
  }

  "RegisterAsMaster" in {
    check(AgentCommand.RegisterAsMaster,
      """{
        "TYPE": "RegisterAsMaster"
      }""".parseJson)
  }

  "Terminate" - {
    "JSON without sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true),
        """{
          "TYPE":"Terminate",
          "sigtermProcesses": true
        }""".parseJson)
    }

    "JSON with sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(30.s)),
        """{
          "TYPE":"Terminate",
          "sigtermProcesses": true,
          "sigkillProcessesAfter": 30
        }""".parseJson)
    }
  }

  "OrderCommand" - {
    "AttachOrder" in {
      check(AgentCommand.AttachOrder(
        Order(
          OrderId("ORDER-ID"),
          NodeKey(JobnetPath("/JOBNET"),NodeId("INPUT")),
          Order.Ready),
        Jobnet(
          JobnetPath("/JOBNET"),
          NodeId("START"),
          List(
            JobNode(NodeId("START"), AgentPath("/AGENT"), JobPath("/JOB"), NodeId("END"), NodeId("END")),
            EndNode(NodeId("END"))))),
        """{
          "TYPE": "AttachOrder",
          "order": {
            "state": {
              "TYPE":
              "Ready"
            },
            "outcome": {
              "returnValue": true
            },
            "variables": {},
            "id": "ORDER-ID",
            "nodeKey": {
              "jobnetPath": "/JOBNET",
              "nodeId": "INPUT"
            }
          },
          "jobnet": {
            "path": "/JOBNET",
            "inputNodeId": "START",
            "idToNode": {
              "START": {
                "agentPath": "/AGENT",
                "onSuccess": "END",
                "id": "START",
                "jobPath": "/JOB",
                "onFailure": "END",
                "TYPE": "JobNode"
              },
              "END": {
                "id": "END",
                "TYPE": "EndNode"
              }
            }
          }
        }""".parseJson)
    }

    "DetachOrder" in {
      check(AgentCommand.DetachOrder(OrderId("ORDER-ID")),
        """{
          "TYPE": "DetachOrder",
          "orderId": "ORDER-ID"
        }""".parseJson)
    }

    "GetOrder" in {
      check(AgentCommand.GetOrder(OrderId("ORDER-ID")),
         """{
          "TYPE": "GetOrder",
          "orderId": "ORDER-ID"
        }""".parseJson)
    }

    "GetOrderIds" in {
      check(AgentCommand.GetOrderIds,
        """{
          "TYPE": "GetOrderIds"
        }""".parseJson)  }
  }

  private def check(command: AgentCommand, json: JsValue): Unit = {
    assert(command.toJson == json)
    assert(command == json.convertTo[AgentCommand])
  }
}
