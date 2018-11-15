package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAwaiting, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalactic.source
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends FreeSpec {

  private val testOrder = Order(
    OrderId("ID"),
    WorkflowPath("/WORKFLOW") % "VERSION",
    Order.Ready,
    payload = Payload(Map(
      "var1" → "value1",
      "var2" → "value2")))

  "JSON" - {
    "Order" in {
      check(
        testOrder.copy(
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
  }

  "Operations" - {
    "attachedToAgent" in {
      val agentId = AgentPath("/A") % "1"
      assert(testOrder.attachedToAgent.isInvalid)
      assert(testOrder.copy(attachedTo = Some(Order.AttachedTo.Agent(agentId)))     .attachedToAgent == Valid(agentId))
      assert(testOrder.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentId))).attachedToAgent.isInvalid)
    }

    "detachableFromAgent" in {
      val agentId = AgentPath("/A") % "1"
      assert(testOrder.detachableFromAgent.isInvalid)
      assert(testOrder.copy(attachedTo = Some(Order.AttachedTo.Agent(agentId)))     .detachableFromAgent.isInvalid)
      assert(testOrder.copy(attachedTo = Some(Order.AttachedTo.Detachable(agentId))).detachableFromAgent == Valid(agentId))
    }

    "castState" in {
      assert(testOrder.castState[Order.Ready] eq testOrder)
      assert(testOrder.castState[Order.Idle] eq testOrder)
      assert(testOrder.castState[Order.State] eq testOrder)
      intercept[ProblemException] {
        testOrder.castState[Order.Processed]
      }
    }

    "ifState" in {
      assert(testOrder.ifState[Order.Ready] == Some(testOrder))
      assert(testOrder.ifState[Order.Idle] == Some(testOrder))
      assert(testOrder.ifState[Order.State] == Some(testOrder))
      assert(testOrder.ifState[Order.Processed] == None)
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
  }

  "Order transitions: event to state" - {
    val freshOnMaster = Order(OrderId("ID"), WorkflowPath("/WORKFLOW") % "VERSION", Order.Fresh())
    val attached = freshOnMaster.copy(attachedTo = Some(Order.AttachedTo.Agent(AgentPath("/AGENT") % "version")))
    val agentId = AgentPath("/AGENT") % "version"
    val allEvents = List[OrderCoreEvent](
      OrderAdded(freshOnMaster.workflowId),
      OrderFinished,
      //OrderStdoutWritten("stdout"),   not an OrderCoreEvent
      //OrderStderrWritten("stderr"),
      OrderAwaiting(OrderId("OFFERED")),
      OrderOffered(OrderId("OFFERED"), until = Timestamp.ofEpochSecond(1)),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderJoined(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderStopped(Outcome.Failed(ReturnCode(1))),
      OrderForked(OrderForked.Child("BRANCH", freshOnMaster.id / "BRANCH") :: Nil),
      OrderTransferredToAgent(agentId),
      OrderTransferredToMaster,
      OrderDetachable,
      OrderDetached)

    "Fresh" - {
      val order = freshOnMaster
      checkAllEvents(order) {
        case _: OrderFinished           ⇒ Order.Finished.getClass
        case _: OrderAwaiting           ⇒ classOf[Order.Awaiting]
        case _: OrderOffered            ⇒ classOf[Order.Processed]
        case _: OrderProcessingStarted  ⇒ Order.InProcess.getClass
        case _: OrderForked             ⇒ classOf[Order.Forked]
        case _: OrderTransferredToAgent ⇒ order.state.getClass
      }
    }

    "Ready" - {
      val order = freshOnMaster
      checkAllEvents(order) {
        case _: OrderFinished           ⇒ Order.Finished.getClass
        case _: OrderAwaiting           ⇒ classOf[Order.Awaiting]
        case _: OrderOffered            ⇒ classOf[Order.Processed]
        case _: OrderProcessingStarted  ⇒ Order.InProcess.getClass
        case _: OrderForked             ⇒ classOf[Order.Forked]
        case _: OrderTransferredToAgent ⇒ order.state.getClass
      }
    }

    "InProcess" - {
      checkAllEvents(attached.copy(state = Order.InProcess)) {
        case _: OrderProcessed ⇒ classOf[Order.Processed]
      }
    }

    "Processed" - {
      checkAllEvents(attached.copy(state = Order.Processed(Outcome.Succeeded(ReturnCode(0))))) {
        case _: OrderMoved ⇒ Order.Ready.getClass
        case _: OrderStopped ⇒ classOf[Order.Stopped]
      }
    }

    "Stopped" - {
      checkAllEvents(attached.copy(state = Order.Stopped(Outcome.Failed(ReturnCode(1))))) {
        PartialFunction.empty  // No way out
      }
    }

    "Forked" - {
      val forked = attached.copy(state = Order.Forked(Order.Forked.Child("BRANCH", attached.id / "CHILD") :: Nil))

      "AttachedTo.Agent" - {
        checkAllEvents(forked) {
          case _: OrderJoined ⇒ classOf[Processed]
          case _: OrderDetachable ⇒ classOf[Forked]
          // case _: OrderDetached ⇒ classOf[Forked]
        }
      }

      "AttachedTo.Detachable" - {
        checkAllEvents(forked.copy(attachedTo = Some(Order.AttachedTo.Detachable(AgentPath("/AGENT") % "1")))) {
          case _: OrderDetached ⇒ classOf[Forked]
          case OrderTransferredToMaster ⇒ classOf[Forked]
        }
      }
    }

    "Offered" - {
      checkAllEvents(attached.copy(state = Order.Offered(Timestamp.ofEpochSecond(1)))) {
        case _: OrderMoved ⇒ Order.Ready.getClass
      }
    }

    "Awaiting" - {
      checkAllEvents(attached.copy(state = Order.Awaiting(OrderId("OFFERED")))) {
        case _: OrderJoined ⇒ classOf[Order.Processed]
      }
    }

    "Finished" - {
      checkAllEvents(attached.copy(state = Order.Finished)) {
        PartialFunction.empty  // No way out
      }
    }

    /** Checks each event in `allEvents`. */
    def checkAllEvents(order: Order[Order.State])(toState: PartialFunction[OrderEvent, Class[_ <: Order.State]])
      (implicit pos: source.Position)
    : Unit =
      for (event ← allEvents) event.toString in {
        assert(order.update(event).toOption.map(_.state.getClass) == toState.lift(event), s"- $event")
      }
  }

  "OrderDetachable" in {
    val detachable = Order(OrderId("ID"), WorkflowPath("/WORKFLOW") % "VERSION", Order.Fresh(), Some(Order.AttachedTo.Detachable(AgentPath("/AGENT") % "version")))
    assert(testOrder.update(OrderDetachable) ==
      Invalid(Problem("Order 'ID' in state 'Ready' (on Master) has received an inapplicable event: OrderDetachable")))
    assert(detachable.update(OrderDetached).isValid)
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
