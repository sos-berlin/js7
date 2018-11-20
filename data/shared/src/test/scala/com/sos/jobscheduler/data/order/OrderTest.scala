package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order.{AttachedTo, Awaiting, Broken, Finished, Forked, Fresh, Idle, InProcess, Offered, Processed, Ready, State, Stopped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttached, OrderAwaiting, OrderBroken, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
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
    Ready,
    payload = Payload(Map(
      "var1" → "value1",
      "var2" → "value2")))

  "JSON" - {
    "Order" in {
      check(
        testOrder.copy(
          attachedTo = Some(AttachedTo.Agent(AgentPath("/AGENT") % "1")),
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

      def check(o: Order[State], json: Json) = testJson(o, json)
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

      "Broken" in {
        check(Broken(Problem("PROBLEM")),
          json"""{
            "TYPE": "Broken",
            "problem": {
              "message": "PROBLEM"
            }
          }""")
      }

      def check(o: State, json: Json) = testJson(o, json)
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

      def check(o: AttachedTo, j: String) = testJson(o, j)
    }
  }

  "Order transitions: event to state" - {
    val orderId = OrderId("ID")
    val workflowId = WorkflowPath("/WORKFLOW") % "VERSION"
    val agentId = AgentPath("/AGENT") % "version"
    val allEvents = Set[OrderCoreEvent](
      OrderAdded(workflowId),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderStopped(Outcome.Failed(ReturnCode(1))),
      OrderMoved(Position(1)),
      OrderForked(OrderForked.Child("BRANCH", orderId / "BRANCH") :: Nil),
      OrderJoined(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderOffered(OrderId("OFFERED"), until = Timestamp.ofEpochSecond(1)),
      OrderAwaiting(OrderId("OFFERED")),
      OrderFinished,
      //OrderStdoutWritten("stdout") is not an OrderCoreEvent
      //OrderStderrWritten("stderr") is not an OrderCoreEvent
      OrderTransferredToAgent(agentId),
      OrderTransferredToMaster,
      OrderAttached(workflowId /: Position(0), Fresh(), None, agentId, Payload.empty),
      OrderDetachable,
      OrderDetached,
      OrderBroken(Problem("Problem")))
    assert(allEvents.map(_.getClass) == OrderEvent.jsonCodec.classes[OrderCoreEvent])
    val detached = none[AttachedTo]
    val attached = Some(AttachedTo.Agent(agentId))
    val detachable = Some(AttachedTo.Detachable(agentId))

    "Fresh" - {
      checkAllEvents(Order(orderId, workflowId, Fresh())) {
        case (_: OrderProcessingStarted , `attached`             ) ⇒ _.isInstanceOf[InProcess]
        case (_: OrderForked            , `detached` | `attached`) ⇒ _.isInstanceOf[Forked]
        case (_: OrderOffered           , `detached`             ) ⇒ _.isInstanceOf[Processed]
        case (_: OrderAwaiting          , `detached`             ) ⇒ _.isInstanceOf[Awaiting]
        case (_: OrderFinished          , `detached`             ) ⇒ _.isInstanceOf[Finished]
        case (_: OrderTransferredToAgent, `detached`             ) ⇒ _.isInstanceOf[Fresh]
        case (OrderTransferredToMaster  , `detachable`           ) ⇒ _.isInstanceOf[Fresh]
        case (OrderDetachable           , `attached`             ) ⇒ _.isInstanceOf[Fresh]
        case (OrderDetached             , `detachable`           ) ⇒ _.isInstanceOf[Fresh]
        case (_: OrderBroken            , _                      ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Ready" - {
      checkAllEvents(Order(orderId, workflowId, Ready)) {
        case (_: OrderProcessingStarted , `attached`             ) ⇒ _.isInstanceOf[InProcess]
        case (_: OrderForked            , `detached` | `attached`) ⇒ _.isInstanceOf[Forked]
        case (_: OrderOffered           , `detached`             ) ⇒ _.isInstanceOf[Processed]
        case (_: OrderAwaiting          , `detached`             ) ⇒ _.isInstanceOf[Awaiting]
        case (_: OrderFinished          , `detached`             ) ⇒ _.isInstanceOf[Finished]
        case (_: OrderTransferredToAgent, `detached`             ) ⇒ _.isInstanceOf[Ready]
        case (OrderTransferredToMaster  , `detachable`           ) ⇒ _.isInstanceOf[Ready]
        case (OrderDetachable           , `attached`             ) ⇒ _.isInstanceOf[Ready]
        case (OrderDetached             , `detachable`           ) ⇒ _.isInstanceOf[Ready]
        case (_: OrderBroken            , _                      ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "InProcess" - {
      checkAllEvents(Order(orderId, workflowId, InProcess)) {
        case (_: OrderProcessed, `attached`) ⇒ _.isInstanceOf[Processed]
        case (_: OrderBroken   , _         ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Processed" - {
      checkAllEvents(Order(orderId, workflowId, Processed(Outcome.Succeeded(ReturnCode(0))))) {
        case (_: OrderMoved  , `detached` | `attached`) ⇒ _.isInstanceOf[Ready]
        case (_: OrderStopped, `attached`             ) ⇒ _.isInstanceOf[Stopped]
        case (_: OrderBroken , _                      ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Stopped" - {
      checkAllEvents(Order(orderId, workflowId, Stopped(Outcome.Failed(ReturnCode(1))))) {
        case (OrderTransferredToMaster, `detachable`         ) ⇒ _.isInstanceOf[Stopped]
        case (_: OrderDetachable      , `attached`           ) ⇒ _.isInstanceOf[Stopped]
        case (_: OrderDetached        , `detachable`         ) ⇒ _.isInstanceOf[Stopped]
        case (_: OrderBroken          , _                    ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Broken" - {
      checkAllEvents(Order(orderId, workflowId, Broken(Problem("PROBLEM")))) {
        case (OrderTransferredToMaster, `detachable`         ) ⇒ _.isInstanceOf[Broken]
        case (_: OrderDetachable      , `attached`           ) ⇒ _.isInstanceOf[Broken]
        case (_: OrderDetached        , `detachable`         ) ⇒ _.isInstanceOf[Broken]
        case (_: OrderBroken          , _                    ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Forked" - {
      checkAllEvents(Order(orderId, workflowId, Forked(Forked.Child("BRANCH", orderId / "CHILD") :: Nil))) {
        case (_: OrderJoined            , `detached`  ) ⇒ _.isInstanceOf[Processed]
        case (_: OrderTransferredToAgent, `detached`  ) ⇒ _.isInstanceOf[Forked]
        case (OrderTransferredToMaster  , `detachable`) ⇒ _.isInstanceOf[Forked]
        case (_: OrderDetachable        , `attached`  ) ⇒ _.isInstanceOf[Forked]
        case (_: OrderDetached          , `detachable`) ⇒ _.isInstanceOf[Forked]
        case (_: OrderBroken            , _           ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Offered" - {
      checkAllEvents(Order(orderId, workflowId, Offered(Timestamp("2018-11-19T12:00:00Z")))) {
        case (_: OrderBroken, _) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Awaiting" - {
      checkAllEvents(Order(orderId, workflowId, Awaiting(OrderId("OFFERED")))) {
        case (_: OrderJoined, `detached`) ⇒ _.isInstanceOf[Processed]
        case (_: OrderBroken, _         ) ⇒ _.isInstanceOf[Broken]
      }
    }

    "Finished" - {
      checkAllEvents(Order(orderId, workflowId, Finished)) {
        case (_: OrderBroken, _) ⇒ _.isInstanceOf[Broken]
      }
    }

    /** Checks each event in `allEvents`. */
    def checkAllEvents(order: Order[State])(toState: PartialFunction[(OrderEvent, Option[AttachedTo]), State ⇒ Boolean])
      (implicit pos: source.Position)
    : Unit =
      for (event ← allEvents) s"$event" - {
        for (a ← None :: attached :: detachable :: Nil) s"$a" in {
          val updated = order.copy(attachedTo = a).update(event)
          val maybeState = updated.toOption.map(_.state)
          val maybePredicate = toState.lift((event, a))
          (maybeState, maybePredicate) match {
            case (Some(state), Some(predicate)) ⇒ assert(predicate(state))
            case (None, None) ⇒
            case _ ⇒ fail
          }
        }
      }
  }

  "Operations" - {
    "attachedToAgent" in {
      val agentId = AgentPath("/A") % "1"
      assert(testOrder.attachedToAgent.isInvalid)
      assert(testOrder.copy(attachedTo = Some(AttachedTo.Agent(agentId)))     .attachedToAgent == Valid(agentId))
      assert(testOrder.copy(attachedTo = Some(AttachedTo.Detachable(agentId))).attachedToAgent.isInvalid)
    }

    "detachableFromAgent" in {
      val agentId = AgentPath("/A") % "1"
      assert(testOrder.detachableFromAgent.isInvalid)
      assert(testOrder.copy(attachedTo = Some(AttachedTo.Agent(agentId)))     .detachableFromAgent.isInvalid)
      assert(testOrder.copy(attachedTo = Some(AttachedTo.Detachable(agentId))).detachableFromAgent == Valid(agentId))
    }

    "castState" in {
      assert(testOrder.castState[Ready] eq testOrder)
      assert(testOrder.castState[Idle] eq testOrder)
      assert(testOrder.castState[State] eq testOrder)
      intercept[ProblemException] {
        testOrder.castState[Processed]
      }
    }

    "ifState" in {
      assert(testOrder.ifState[Ready] == Some(testOrder))
      assert(testOrder.ifState[Idle] == Some(testOrder))
      assert(testOrder.ifState[State] == Some(testOrder))
      assert(testOrder.ifState[Processed] == None)
    }

    "isAttachable" in {
      val order = Order(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW") % "VERSION", Ready,
        Some(AttachedTo.Detachable(AgentPath("/AGENT") % "1")))
      assert(order.detachableFromAgent == Valid(AgentPath("/AGENT") % "1"))

      for (o ← Array(
            order.copy(attachedTo = Some(AttachedTo.Agent(AgentPath("/AGENT") % "1"))),
            order.copy(attachedTo = None))) {
        val problem = o.detachableFromAgent.asInstanceOf[Invalid[Problem]].e
        assert(problem.toString contains "ORDER-ID")
      }
    }
  }

  "Error message when updated failed" in {
    assert(testOrder.update(OrderDetachable) ==
      Invalid(Problem("Order 'ID' in state 'Ready' (on Master) has received an inapplicable event: OrderDetachable")))
  }

  if (sys.props contains "test.speed") "Speed" in {
    val order = Order(OrderId("ORDER-1"), (WorkflowPath("/WORKFLOW") % "VERSION") /: Position(1), Ready,
      Some(AttachedTo.Agent(AgentPath("/AGENT") % "1")))
    val json = (order: Order[State]).asJson
    testSpeed(100000, "asOrder")(json.as[Order[State]])
    def testSpeed(n: Int, ops: String)(what: ⇒ Unit): Unit = {
      val start = Timestamp.currentTimeMillis
      for (_ ← 1 to n) what
      val duration = Timestamp.currentTimeMillis - start
      println(s"${duration}ms/$n $ops ${(n * 1000L / duration).toString} $ops/s")
    }
  }
}
