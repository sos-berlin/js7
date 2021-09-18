package js7.agent.data.commands

import io.circe.Json
import java.util.UUID
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand.{Batch, DetachOrder, NoOperation, ShutDown}
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.silly.SillySigner
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.problem.TestCodeProblem
import js7.common.message.ProblemCodeMessages
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.command.CancellationMode
import js7.data.controller.ControllerId
import js7.data.event.JournalId
import js7.data.item.{ItemRevision, ItemSigner, VersionId}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.StringValue
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting.SimpleTestWorkflow
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentCommandTest extends AnyFreeSpec
{
  ProblemCodeMessages.initialize()

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
            "message": "TestMessage argument=$$argument (ARG=VALUE)"
          }
        ]
      }""")
  }

  "MarkOrder" in {
    check(AgentCommand.MarkOrder(OrderId("ORDER"), OrderMark.Cancelling(CancellationMode.FreshOnly)),
      json"""{
        "TYPE": "MarkOrder",
        "orderId": "ORDER",
        "mark": {
          "TYPE": "Cancelling",
          "mode": {
            "TYPE": "FreshOnly"
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

  "DedicateAgent" in {
    check(AgentCommand.DedicateAgent(AgentPath("AGENT"), ControllerId("CONTROLLER")),
      json"""{
        "TYPE": "DedicateAgent",
        "agentPath": "AGENT",
        "controllerId": "CONTROLLER"
      }""")
  }

  "DedicateAgent.Response" in {
    testJson[AgentCommand.Response](
      AgentCommand.DedicateAgent.Response(
        AgentRunId(JournalId(UUID.fromString("11111111-2222-3333-4444-555555555555"))),
        1000L),
      json"""{
        "TYPE": "DedicateAgent.Response",
        "agentRunId": "ERERESIiMzNERFVVVVVVVQ",
        "agentEventId": 1000
      }""")
  }

  "CoupleController" in {
    check(
      AgentCommand.CoupleController(
        AgentPath("AGENT"),
        AgentRunId(JournalId(UUID.fromString("11111111-2222-3333-4444-555555555555"))),
        1000L),
      json"""{
        "TYPE": "CoupleController",
        "agentPath": "AGENT",
        "agentRunId": "ERERESIiMzNERFVVVVVVVQ",
        "eventId": 1000
      }""")
  }

  "Reset" in {
    check(
      AgentCommand.Reset(
        Some(AgentRunId(JournalId(UUID.fromString("11111111-2222-3333-4444-555555555555"))))),
      json"""{
        "TYPE": "Reset",
        "agentRunId": "ERERESIiMzNERFVVVVVVVQ"
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

    "JSON without sigkillDelay" in {
      check(AgentCommand.ShutDown(processSignal = Some(SIGTERM)),
        json"""{
          "TYPE": "ShutDown",
          "processSignal": "SIGTERM",
          "suppressSnapshot": false,
          "restart": false
        }""")
    }

    "JSON with restart" in {
      check(AgentCommand.ShutDown(restart = true),
        json"""{
          "TYPE": "ShutDown",
          "restart": true,
          "suppressSnapshot": false
        }""")
    }
  }

  "AttachItem" in {
    check(
      AgentCommand.AttachItem(
        FileWatch(OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"), "DIRECTORY")),
      json"""{
        "TYPE": "AttachItem",
        "item": {
          "TYPE": "FileWatch",
          "path": "PATH",
          "workflowPath": "WORKFLOW",
          "agentPath": "AGENT",
          "directory": "DIRECTORY",
          "delay": 0
        }
      }""")
  }

  "AttachSignedItem VersionedItem (without ItemRevision)" in {
    val itemSigner = new ItemSigner(SillySigner.Default, AgentState.signableItemJsonCodec)
    check(
      AgentCommand.AttachSignedItem(
        itemSigner.sign(SimpleTestWorkflow)),
      json"""{
        "TYPE": "AttachSignedItem",
        "signed": {
          "signature": {
            "TYPE": "Silly",
            "signatureString": "SILLY-SIGNATURE"
          },
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"VERSION\",\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"A.cmd\",\"v1Compatible\":true},\"defaultArguments\":{\"JOB_A\":\"'A-VALUE'\"},\"parallelism\":3}},{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"PathExecutable\",\"path\":\"B.cmd\",\"v1Compatible\":true},\"defaultArguments\":{\"JOB_B\":\"'B-VALUE'\"},\"parallelism\":3}}]}"
        }
      }""")
  }

  "AttachSignedItem JobResource (with ItemRevision)" in {
    val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"), itemRevision = Some(ItemRevision(7)))
    val itemSigner = new ItemSigner(SillySigner.Default, AgentState.signableItemJsonCodec)
    check(
      AgentCommand.AttachSignedItem(itemSigner.sign(jobResource)),
      json"""{
        "TYPE": "AttachSignedItem",
        "signed": {
          "signature": {
            "TYPE": "Silly",
            "signatureString": "SILLY-SIGNATURE"
          },
          "string": "{\"TYPE\":\"JobResource\",\"path\":\"JOB-RESOURCE\",\"variables\":{},\"env\":{}}"
        },
        "itemRevision": 7
       }""")
  }

  "DetachItem" in {
    check(
      AgentCommand.DetachItem(OrderWatchPath("PATH")),
      json"""{
        "TYPE": "DetachItem",
        "key": "OrderWatch:PATH"
      }""")

    check(
      AgentCommand.DetachItem(WorkflowPath("WORKFLOW") ~ VersionId("1")),
      json"""{
        "TYPE": "DetachItem",
        "key": "Workflow:WORKFLOW~1"
      }""")
  }

  "OrderCommand" - {
    "AttachOrder" in {
      check(AgentCommand.AttachOrder(
          Order(
            OrderId("ORDER-ID"),
            SimpleTestWorkflow.id /: Position(3),
            Order.Ready,
            Map("KEY" -> StringValue("VALUE"))),
          AgentPath("AGENT")),
        json"""{
          "TYPE": "AttachOrder",
          "order": {
            "id": "ORDER-ID",
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
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
              "agentPath": "AGENT"
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
