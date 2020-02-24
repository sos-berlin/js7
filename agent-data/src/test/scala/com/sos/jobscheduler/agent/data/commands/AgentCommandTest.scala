package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Batch, DetachOrder, NoOperation, ShutDown}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.TestCodeProblem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignedString}
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
final class AgentCommandTest extends FreeSpec
{
  "Batch" in {
    check(AgentCommand.Batch(List(AgentCommand.NoOperation, AgentCommand.EmergencyStop())),
      json"""{
        "TYPE": "Batch",
        "commands": [
          { "TYPE": "NoOperation" },
          { "TYPE": "EmergencyStop" }
        ]
      }""")
  }

  "Batch.Response" in {
    testJson[AgentCommand.Response](
      AgentCommand.Batch.Response(Right(AgentCommand.Response.Accepted) :: Left(TestCodeProblem(Map("ARG" -> "VALUE"))) :: Nil),
      json"""{
        "TYPE": "BatchResponse",
        "responses": [
          { "TYPE": "Accepted" },
          {
            "TYPE": "Problem",
            "code": "TestCode",
            "arguments": {
              "ARG": "VALUE"
            },
            "message": "TestCode (ARG=VALUE)"
          }
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

  "EmergencyStop" - {
    "restart=false" in {
      check(AgentCommand.EmergencyStop(),
        json"""{ "TYPE": "EmergencyStop" }""")
    }

    "restart=true" in {
      check(AgentCommand.EmergencyStop(restart = true),
        json"""{
          "TYPE": "EmergencyStop",
          "restart": true
        }""")
    }
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

  "ShutDown" - {
    "JSON without sigkillProcessesAfter" in {
      check(AgentCommand.ShutDown(sigtermProcesses = true),
        json"""{
          "TYPE": "ShutDown",
          "sigtermProcesses": true
        }""")
    }

    "JSON with sigkillProcessesAfter" in {
      check(AgentCommand.ShutDown(sigtermProcesses = true, sigkillProcessesAfter = Some(30.seconds)),
        json"""{
          "TYPE": "ShutDown",
          "sigtermProcesses": true,
          "sigkillProcessesAfter": 30
        }""")
    }

    "JSON with restart" in {
      check(AgentCommand.ShutDown(restart = true),
        json"""{
          "TYPE": "ShutDown",
          "restart": true
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
            Map("KEY" -> "VALUE")),
          AgentRefPath("/AGENT"),
          SignedString("""{"TYPE":"Workflow",...}""", GenericSignature("Silly", "MY-SILLY-SIGNATURE"))),
        json"""{
          "TYPE": "AttachOrder",
          "order": {
            "id": "ORDER-ID",
            "workflowPosition": {
              "workflowId": {
                "path": "/WORKFLOW",
                "versionId": "VERSION"
                },
              "position": [ 3 ]
            },
            "state": {
              "TYPE": "Ready"
            },
            "arguments": {
              "KEY": "VALUE"
            },
            "attachedState": {
              "TYPE": "Attached",
              "agentRefPath": "/AGENT"
            },
            "historicOutcomes": []
          },
          "signedWorkflow": {
            "string": "{\"TYPE\":\"Workflow\",...}",
            "signature": {
              "TYPE": "Silly",
              "signatureString": "MY-SILLY-SIGNATURE"
            }
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

    "GetOrders" in {
      check(AgentCommand.GetOrders,
         json"""{
          "TYPE": "GetOrders"
        }""")
    }
  }

  "Batch toString" in {
    assert(Batch(Nil).toString == "Batch()")
    assert(Batch(DetachOrder(OrderId("A")) :: Nil).toString == "Batch(DetachOrder)")
    assert(
      Batch(
        DetachOrder(OrderId("A")) :: DetachOrder(OrderId("A")) ::
        ShutDown() ::
        NoOperation :: NoOperation :: NoOperation :: Nil
      ).toString == "Batch(2×DetachOrder, ShutDown, 3×NoOperation)")
  }

  private def check(command: AgentCommand, json: Json): Unit =
    testJson(command, json)
}
