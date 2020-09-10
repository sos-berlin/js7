package js7.agent.data.commands

import io.circe.Json
import java.util.UUID
import js7.agent.data.commands.AgentCommand.{Batch, DetachOrder, NoOperation, ShutDown}
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.{GenericSignature, SignedString}
import js7.base.problem.TestCodeProblem
import js7.base.process.ProcessSignal.SIGTERM
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.command.CancelMode
import js7.data.event.JournalId
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting.SimpleTestWorkflow
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentCommandTest extends AnyFreeSpec
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
        "TYPE": "Batch.Response",
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

  "MarkOrder" in {
    check(AgentCommand.MarkOrder(OrderId("ORDER"), OrderMark.Cancelling(CancelMode.NotStarted)),
      json"""{
        "TYPE": "MarkOrder",
        "orderId": "ORDER",
        "mark": {
          "TYPE": "Cancelling",
          "mode": {
            "TYPE": "NotStarted"
          }
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

  "ReleaseEvents" in {
    check(AgentCommand.ReleaseEvents(123L),
      json"""{
        "TYPE": "ReleaseEvents",
        "untilEventId": 123
      }""")
  }

  "NoOperation" in {
    check(AgentCommand.NoOperation,
      json"""{
        "TYPE": "NoOperation"
      }""")
  }

  "RegisterAsController" in {
    check(AgentCommand.RegisterAsController(AgentRefPath("/AGENT")),
      json"""{
        "TYPE": "RegisterAsController",
        "agentRefPath": "/AGENT"
      }""")
  }

  "RegisterAsController.Response" in {
    testJson[AgentCommand.Response](
      AgentCommand.RegisterAsController.Response(AgentRunId(JournalId(UUID.fromString("11111111-2222-3333-4444-555555555555")))),
      json"""{
        "TYPE": "RegisterAsController.Response",
        "agentRunId": "ERERESIiMzNERFVVVVVVVQ"
      }""")
  }

  "CoupleController" in {
    check(
      AgentCommand.CoupleController(
        AgentRefPath("/AGENT"),
        AgentRunId(JournalId(UUID.fromString("11111111-2222-3333-4444-555555555555"))),
        1000L),
      json"""{
        "TYPE": "CoupleController",
        "agentRefPath": "/AGENT",
        "agentRunId": "ERERESIiMzNERFVVVVVVVQ",
        "eventId": 1000
      }""")
  }

  "CoupleController.Response" in {
    testJson[AgentCommand.Response](
      AgentCommand.CoupleController.Response(Set(OrderId("ORDER"))),
      json"""{
        "TYPE": "CoupleController.Response",
        "orderIds": [ "ORDER" ]
      }""")
  }

  "ShutDown" - {
    "using defaults" in {
      testJsonDecoder[AgentCommand](AgentCommand.ShutDown(),
        json"""{
          "TYPE": "ShutDown"
        }""")
    }

    "JSON without sigkillAfter" in {
      check(AgentCommand.ShutDown(processSignal = Some(SIGTERM)),
        json"""{
          "TYPE": "ShutDown",
          "processSignal": "SIGTERM",
          "restart": false
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
