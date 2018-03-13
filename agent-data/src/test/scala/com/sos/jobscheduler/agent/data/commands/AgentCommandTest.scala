package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.test.TestSetting.SimpleTestWorkflow
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import org.scalatest.FreeSpec
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class AgentCommandTest extends FreeSpec {

  "Batch" in {
    check(AgentCommand.Batch(List(AgentCommand.NoOperation, AgentCommand.Logout)),
      json"""{
        "TYPE": "Batch",
        "commands": [
          { "TYPE": "NoOperation" },
          { "TYPE": "Logout" }
        ]
      }""")
  }

  "EmergencyStop" in {
    check(AgentCommand.EmergencyStop,
      json"""{ "TYPE": "EmergencyStop" }""")
  }

  "Login" in {
    check(AgentCommand.Login,
      json"""{
        "TYPE": "Login"
      }""")
  }

  "Logout" in {
    check(AgentCommand.Logout,
      json"""{
        "TYPE": "Logout"
      }""")
  }

  "NoOperation" in {
    check(AgentCommand.NoOperation,
      json"""{
        "TYPE": "NoOperation"
      }""")
  }

  "RegisterAsMaster" in {
    check(AgentCommand.RegisterAsMaster,
      json"""{
        "TYPE": "RegisterAsMaster"
      }""")
  }

  "Terminate" - {
    "JSON without sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true),
        json"""{
          "TYPE":"Terminate",
          "sigtermProcesses": true
        }""")
    }

    "JSON with sigkillProcessesAfter" in {
      check(AgentCommand.Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(30.seconds)),
        json"""{
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
            SimpleTestWorkflow.id /: Position(3),
            Order.Ready,
            Some(Order.AttachedTo.Agent(AgentPath("/AGENT")))),
          AgentPath("/AGENT"),
          SimpleTestWorkflow),
        json"""{
          "TYPE": "AttachOrder",
          "order": {
            "state": {
              "TYPE": "Ready"
            },
            "payload": {
              "variables": {}
            },
            "attachedTo": {
              "agentPath": "/AGENT",
              "TYPE": "Agent"
            },
            "id": "ORDER-ID",
            "workflowPosition": {
              "workflowId": {
                "path": "/WORKFLOW",
                "versionId": "VERSION"
                },
              "position": [ 3 ]
            }
          },
          "workflow": {
            "id": {
              "path": "/WORKFLOW",
              "versionId": "VERSION"
            },
            "instructions": [
              { "TYPE": "Job", "jobPath": "/A", "agentPath": "/AGENT" },
              { "TYPE": "Job", "jobPath": "/B", "agentPath": "/AGENT" }
            ]
          }
        }""")
    }

    "DetachOrder" in {
      check(AgentCommand.DetachOrder(OrderId("ORDER-ID")),
        json"""{
          "TYPE": "DetachOrder",
          "orderId": "ORDER-ID"
        }""")
    }

    "GetOrder" in {
      check(AgentCommand.GetOrder(OrderId("ORDER-ID")),
         json"""{
          "TYPE": "GetOrder",
          "orderId": "ORDER-ID"
        }""")
    }

    "GetOrderIds" in {
      check(AgentCommand.GetOrderIds,
        json"""{
          "TYPE": "GetOrderIds"
        }""")
    }
  }

  private def check(command: AgentCommand, json: Json): Unit =
    testJson(command, json)
}
