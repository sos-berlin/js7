package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
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
      val agentId = AgentPath("/A") % "1"
      assert(order.attachedToAgent.isInvalid)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Agent(agentId)))     .attachedToAgent == Valid(agentId))
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentId))).attachedToAgent.isInvalid)
    }

    "detachableFromAgent" in {
      val agentId = AgentPath("/A") % "1"
      assert(order.detachableFromAgent.isInvalid)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Agent(agentId)))     .detachableFromAgent.isInvalid)
      assert(order.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentId))).detachableFromAgent == Valid(agentId))
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
          attachedTo = Some(Order.AttachedTo.Agent(AgentPath("/AGENT") % "1")),
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
            "agentId": {
              "path": "/AGENT",
              "versionId": "1"
            }
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
    "Fresh scheduled" in {
      check(Fresh(Some(Timestamp.parse("2017-11-15T12:33:44.789Z"))),
        json"""{
          "TYPE": "Fresh",
          "scheduledAt": 1510749224789
        }""")
    }

    "Fresh immediately" in {
      check(Fresh(),
        json"""{
          "TYPE": "Fresh"
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

    "Forked" in {
      check(Forked(List(
        Forked.Child(BranchId("A"), OrderId("A/1"), MapDiff(Map("K" → "V"))),
        Forked.Child(BranchId("B"), OrderId("B/1")))),
        json"""{
          "TYPE": "Forked",
            "children": [
              {
                "branchId": "A",
                "orderId": "A/1",
                "variablesDiff": {
                  "changed": { "K": "V" },
                  "deleted": []
                }
              }, {
                "branchId": "B",
                "orderId": "B/1",
                "variablesDiff": {
                  "changed": {},
                  "deleted": []
                }
              }
            ]
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
    val order = Order(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW") % "VERSION", Order.Ready,
      Some(AttachedTo.Detachable(AgentPath("/AGENT") % "1")))
    assert(order.detachableFromAgent == Valid(AgentPath("/AGENT") % "1"))

    for (o ← Array(
          order.copy(attachedTo = Some(Order.AttachedTo.Agent(AgentPath("/AGENT") % "1"))),
          order.copy(attachedTo = None))) {
      val problem = o.detachableFromAgent.asInstanceOf[Invalid[Problem]].e
      assert(problem.toString contains "ORDER-ID")
    }
  }

  "AttachedTo" - {
    "Agent" in {
      check(AttachedTo.Agent(AgentPath("/AGENT") % "1"),
        """{
          "TYPE": "Agent",
          "agentId": {
            "path": "/AGENT",
            "versionId": "1"
          }
        }""")
    }

    "Detachable" in {
      check(AttachedTo.Detachable(AgentPath("/AGENT") % "1"),
        """{
          "TYPE": "Detachable",
          "agentId": {
            "path": "/AGENT",
            "versionId": "1"
          }
        }""")
    }

    def check(o: Order.AttachedTo, j: String) = testJson(o, j)
  }

  if (sys.props contains "test.speed") "Speed" in {
    val order = Order(OrderId("ORDER-1"), (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(1), Order.Ready,
      Some(Order.AttachedTo.Agent(AgentPath("/AGENT") % "1")))
    val json = (order: Order[Order.State]).asJson
    testSpeed(100000, "asOrder")(json.as[Order[Order.State]])
    def testSpeed(n: Int, ops: String)(what: ⇒ Unit): Unit = {
      val start = Timestamp.currentTimeMillis
      for (_ ← 1 to n) what
      val duration = Timestamp.currentTimeMillis - start
      println(s"${duration}ms/$n $ops ${(n * 1000L / duration).toString} $ops/s")
    }
  }
}
