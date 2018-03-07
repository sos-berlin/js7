package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends FreeSpec {

  "Order" - {
    val order = Order(
      OrderId("ID"),
      WorkflowPath("/WORKFLOW") % "VERSION",
      Order.Ready,
      payload = Payload(Map(
        "var1" → "value1",
        "var2" → "value2")))

    "OrderDetachable" in {
      intercept[IllegalStateException] {
        order.update(OrderEvent.OrderDetachable)
      }
    }

    "attachedToAgent" in {
      val agentPath = AgentPath("/A")
      assert(order.attachedToAgent.isInvalid)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Agent(agentPath)))     .attachedToAgent == Valid(agentPath))
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentPath))).attachedToAgent.isInvalid)
    }

    "detachableFromAgent" in {
      val agentPath = AgentPath("/A")
      assert(order.detachableFromAgent.isInvalid)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Agent(agentPath)))     .detachableFromAgent.isInvalid)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentPath))).detachableFromAgent == Valid(agentPath))
    }

    "castState" in {
      assert(order.castState[Order.Ready] eq order)
      assert(order.castState[Order.Idle] eq order)
      assert(order.castState[Order.State] eq order)
      intercept[ProblemException] {
        order.castState[Order.Processed]
      }
    }

    "ifState" in {
      assert(order.ifState[Order.Ready] == Some(order))
      assert(order.ifState[Order.Idle] == Some(order))
      assert(order.ifState[Order.State] == Some(order))
      assert(order.ifState[Order.Processed] == None)
    }

    "JSON" in {
      check(
        order.copy(
          attachedTo = Some(Order.AttachedTo.Agent(AgentPath("/AGENT"))),
          parent = Some(OrderId("PARENT"))),
        json"""{
          "id": "ID",
          "workflowPosition": {
            "workflowId": {
              "path": "/WORKFLOW",
              "versionId": "VERSION"
            },
            "position": [ 0 ]
          },
          "state": {
            "TYPE": "Ready"
          },
          "attachedTo": {
            "TYPE": "Agent",
            "agentPath": "/AGENT"
          },
          "parent": "PARENT",
          "payload": {
            "variables": {
              "var1": "value1",
              "var2": "value2"
            }
          }
        }""")

      def check(o: Order[Order.State], json: Json) = testJson(o, json)
    }
  }

  "State" - {
    "Scheduled" in {
      check(Scheduled(Timestamp.parse("2017-11-15T12:33:44.789Z")),
        json"""{
          "TYPE": "Scheduled",
          "at": 1510749224789
        }""")
    }

    "StartNow" in {
      check(StartNow,
        json"""{
          "TYPE": "StartNow"
        }""")
    }

    "Ready" in {
      check(Ready,
        json"""{
          "TYPE": "Ready"
        }""")
    }

    "Stopped" in {
      check(Stopped(Outcome.Failed(ReturnCode(1))),
        json"""{
          "TYPE": "Stopped",
          "outcome": {
            "TYPE": "Failed",
            "returnCode": 1
          }
        }""")
    }

    "InProcess" in {
      check(InProcess,
        json"""{
          "TYPE": "InProcess"
        }""")
    }

    "Processed" in {
      check(Processed(Outcome.Succeeded(ReturnCode(7))),
        json"""{
          "TYPE": "Processed",
          "outcome": {
            "TYPE": "Succeeded",
            "returnCode": 7
          }
        }""")
    }

    "Join" in {
      check(Join(List(OrderId("A/1"), OrderId("A/2"))),
        json"""{
          "TYPE": "Join",
          "joinOrderIds": [ "A/1", "A/2" ]
        }""")
    }

    "Offered" in {
      check(Offered(Timestamp.ofEpochMilli(123)),
        json"""{
          "TYPE": "Offered",
          "until": 123
        }""")
    }

    "Finished" in {
      check(Finished,
        json"""{
          "TYPE": "Finished"
        }""")
    }

    def check(o: Order.State, json: Json) = testJson(o, json)
  }

  "isAttachable" in {
    val order = Order(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW") % "VERSION", Order.Ready, Some(AttachedTo.Detachable(AgentPath("/AGENT"))))
    assert(order.detachableFromAgent == Valid(AgentPath("/AGENT")))

    for (o ← Array(
          order.copy(attachedTo = Some(Order.AttachedTo.Agent(AgentPath("/AGENT")))),
          order.copy(attachedTo = None))) {
      val problem = o.detachableFromAgent.asInstanceOf[Invalid[Problem]].e
      assert(problem.toString contains "ORDER-ID")
    }
  }

  "AttachedTo" - {
    "Agent" in {
      check(AttachedTo.Agent(AgentPath("/AGENT")),
        """{
          "TYPE": "Agent",
          "agentPath": "/AGENT"
        }""")
    }

    "Detachable" in {
      check(AttachedTo.Detachable(AgentPath("/AGENT")),
        """{
          "TYPE": "Detachable",
          "agentPath": "/AGENT"
        }""")
    }

    def check(o: Order.AttachedTo, j: String) = testJson(o, j)
  }

  if (sys.props contains "test.speed") "Speed" in {
    val order = Order(OrderId("ORDER-1"), (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(1), Order.Ready, Some(Order.AttachedTo.Agent(AgentPath("/AGENT"))))
    val json = (order: Order[Order.State]).asJson
    testSpeed(100000, "asOrder")(json.as[Order[Order.State]])
    def testSpeed(n: Int, ops: String)(what: ⇒ Unit): Unit = {
      val start = Timestamp.epochMilli
      for (_ ← 1 to n) what
      val duration = Timestamp.epochMilli - start
      println(s"${duration}ms/$n $ops ${(n * 1000L / duration).toString} $ops/s")
    }
  }
}
