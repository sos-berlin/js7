package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.position.Position
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
    check(AgentCommand.Batch(List(AgentCommand.NoOperation, AgentCommand.EmergencyStop)),
      json"""{
        "TYPE": "Batch",
        "commands": [
          { "TYPE": "NoOperation" },
          { "TYPE": "EmergencyStop" }
        ]
      }""")
  }

  "CancelOrder" in {
    check(AgentCommand.CancelOrder(OrderId("ORDER"), CancelMode.NotStarted),
      json"""{
        "TYPE": "CancelOrder",
        "orderId": "ORDER",
        "mode": {
          "TYPE": "NotStarted"
        }
      }""")
  }

  "EmergencyStop" in {
    check(AgentCommand.EmergencyStop,
      json"""{ "TYPE": "EmergencyStop" }""")
  }

  "KeepEvents" in {
    check(AgentCommand.KeepEvents(123),
      json"""{
        "TYPE": "KeepEvents",
        "after": 123
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
            Order.Ready),
          AgentPath("/AGENT") % "1",
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
            "attachedState": {
              "TYPE": "Attached",
              "agentId": {
                "path": "/AGENT",
                "versionId": "1"
              }
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
              {
                "TYPE": "Execute.Anonymous",
                "job": {
                  "agentPath": "/AGENT",
                  "defaultArguments": {
                    "JOB_A": "A-VALUE"
                  },
                  "executablePath": "/A.cmd",
                  "taskLimit": 8
                }
              },
              {
                "TYPE": "Execute.Anonymous",
                "job": {
                  "agentPath": "/AGENT",
                  "defaultArguments": {
                    "JOB_B": "B-VALUE"
                  },
                  "executablePath": "/B.cmd",
                  "taskLimit": 8
                }
              }
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
