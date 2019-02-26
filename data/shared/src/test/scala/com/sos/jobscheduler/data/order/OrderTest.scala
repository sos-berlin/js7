package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order.{Attached, AttachedState, Attaching, Awaiting, Broken, Detaching, Finished, Forked, Fresh, FreshOrReady, Offering, Processed, Processing, Ready, State, Stopped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAwaiting, OrderBroken, OrderCancelationMarked, OrderCanceled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderRetrying, OrderStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalactic.source
import org.scalatest.FreeSpec
import scala.collection.immutable.ListSet
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends FreeSpec
{
  private val testOrder = Order(
    OrderId("ID"),
    WorkflowPath("/WORKFLOW") % "VERSION",
    Ready,
    payload = Payload(Map(
      "var1" → "value1",
      "var2" → "value2")))

  "JSON" - {
    "Order" - {
      "Ready" in {
        check(
          testOrder.copy(
            attachedState = Some(Attached(AgentPath("/AGENT") % "1")),
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
            "attachedState": {
              "TYPE": "Attached",
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
      }

      "cancel" in {
        check(
          Order(OrderId("ID"), WorkflowPath("/WORKFLOW") % "VERSION", Fresh(), cancel = Some(CancelMode.NotStarted)),
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
              "TYPE": "Fresh"
            },
            "payload": {
              "variables": {}
            },
            "cancel": {
              "TYPE": "NotStarted"
            }
          }""")
      }

      def check(o: Order[State], json: Json) = testJson(o, json)
    }

    "State" - {
      "Fresh scheduled" in {
        check(Fresh(Some(Timestamp.parse("2017-11-15T12:33:44.789Z"))),
          json"""{
            "TYPE": "Fresh",
            "scheduledFor": 1510749224789
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

      "Processing" in {
        check(Processing,
          json"""{
            "TYPE": "Processing"
          }""")
      }

      "Processed" in {
        check(Processed,
          json"""{
            "TYPE": "Processed"
          }""")
      }

      "Stopped" in {
        check(Stopped,
          json"""{
            "TYPE": "Stopped"
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

      "Offering" in {
        check(Offering(Timestamp.ofEpochMilli(123)),
          json"""{
            "TYPE": "Offering",
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

    "AttachedState" - {
      "Attached" in {
        check(Attached(AgentPath("/AGENT") % "1"),
          """{
            "TYPE": "Attached",
            "agentId": {
              "path": "/AGENT",
              "versionId": "1"
            }
          }""")
      }

      "Detaching" in {
        check(Detaching(AgentPath("/AGENT") % "1"),
          """{
            "TYPE": "Detaching",
            "agentId": {
              "path": "/AGENT",
              "versionId": "1"
            }
          }""")
      }

      def check(o: AttachedState, j: String) = testJson(o, j)
    }
  }

  "Order transitions: event to state" - {
    val orderId = OrderId("ID")
    val workflowId = WorkflowPath("/WORKFLOW") % "VERSION"
    val agentId = AgentPath("/AGENT") % "version"
    val allEvents = ListSet[OrderCoreEvent](
      OrderAdded(workflowId),

      OrderAttachable(agentId.path),
      OrderAttached(workflowId /: Position(0), Fresh(), Outcome.succeeded, None, agentId, Payload.empty),
      OrderTransferredToAgent(agentId),

      OrderStarted,
      OrderProcessingStarted,
      //OrderStdoutWritten("stdout") is not an OrderCoreEvent
      //OrderStderrWritten("stderr") is not an OrderCoreEvent
      OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderStopped(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(1)),
      OrderRetrying(Position(1)),
      OrderMoved(Position(1)),
      OrderForked(OrderForked.Child("BRANCH", orderId / "BRANCH") :: Nil),
      OrderJoined(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderOffered(OrderId("OFFERED"), until = Timestamp.ofEpochSecond(1)),
      OrderAwaiting(OrderId("OFFERED")),
      OrderFinished,

      OrderCancelationMarked(CancelMode.NotStarted),
      OrderCanceled,
      OrderBroken(Problem("Problem")),

      OrderDetachable,
      OrderDetached,
      OrderTransferredToMaster
    )

    assert(allEvents.map(_.getClass) == OrderEvent.jsonCodec.classes[OrderCoreEvent])
    val detached  = none[AttachedState]
    val attaching = Some(Attaching(agentId.path))
    val attached  = Some(Attached(agentId))
    val detaching = Some(Detaching(agentId))

    "Fresh" - {
      checkAllEvents(Order(orderId, workflowId, Fresh()),
        attachingAllowed[Fresh] orElse
        detachingAllowed[Fresh] orElse {
          case (_: OrderMoved            , `detached` | `attached`              ) ⇒ _.isInstanceOf[Fresh]
          case (_: OrderStarted          , `detached` | `attached`              ) ⇒ _.isInstanceOf[Ready]
          case (_: OrderCancelationMarked, `detached` | `attaching` | `attached`) ⇒ _.isInstanceOf[Fresh]
          case (OrderCanceled            , `detached`                           ) ⇒ _.isInstanceOf[Order.Canceled]
          case (_: OrderBroken           , _                                    ) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Ready" - {
      checkAllEvents(Order(orderId, workflowId, Ready),
        attachingAllowed[Ready] orElse
        detachingAllowed[Ready] orElse
        cancelationMarkingAllowed[Ready] orElse {
          case (_: OrderMoved            , `detached` | `attached`) ⇒ _.isInstanceOf[Ready]
          case (_: OrderProcessingStarted, `attached`             ) ⇒ _.isInstanceOf[Processing]
          case (_: OrderForked           , `detached` | `attached`) ⇒ _.isInstanceOf[Forked]
          case (_: OrderOffered          , `detached`             ) ⇒ _.isInstanceOf[Processed]
          case (_: OrderAwaiting         , `detached`             ) ⇒ _.isInstanceOf[Awaiting]
          case (_: OrderCatched          , `detached` | `attached`) ⇒ _.isInstanceOf[Ready]     // Fail instruction
          case (_: OrderStopped          , `detached` | `attached`) ⇒ _.isInstanceOf[Stopped]   // Fail instruction
          case (_: OrderRetrying         , `detached` | `attached`) ⇒ _.isInstanceOf[Ready]
          case (_: OrderFinished         , `detached`             ) ⇒ _.isInstanceOf[Finished]
          case (OrderCanceled            , `detached`             ) ⇒ _.isInstanceOf[Order.Canceled]
          case (_: OrderBroken           , _                      ) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Processing" - {
      checkAllEvents(Order(orderId, workflowId, Processing),
        cancelationMarkingAllowed[Processing] orElse {
          case (_: OrderProcessed, `attached`) ⇒ _.isInstanceOf[Processed]
          case (_: OrderBroken   , _         ) ⇒ _.isInstanceOf[Broken]
      })
    }

    "Processed" - {
      checkAllEvents(Order(orderId, workflowId, Processed, Outcome.Succeeded(ReturnCode(0))),
        cancelationMarkingAllowed[Processed] orElse {
          case (_: OrderMoved  , `detached` | `attached`) ⇒ _.isInstanceOf[Ready]
          case (_: OrderStopped, `attached`             ) ⇒ _.isInstanceOf[Stopped]
          case (_: OrderCatched, `attached`             ) ⇒ _.isInstanceOf[Ready]
          case (_: OrderBroken , _                      ) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Stopped" - {
      checkAllEvents(Order(orderId, workflowId, Stopped, outcome = Outcome.Failed(ReturnCode(1))),
        detachingAllowed[Stopped] orElse
        cancelationMarkingAllowed[Stopped] orElse {
          case (OrderCanceled , `detached`) ⇒ _.isInstanceOf[Order.Canceled]
          case (_: OrderBroken, _         ) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Broken" - {
      checkAllEvents(Order(orderId, workflowId, Broken(Problem("PROBLEM"))),
        detachingAllowed[Broken] orElse
        cancelationMarkingAllowed[Broken] orElse {
          case (OrderCanceled , `detached`) ⇒ _.isInstanceOf[Order.Canceled]
          case (_: OrderBroken, _         ) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Forked" - {
      checkAllEvents(Order(orderId, workflowId, Forked(Forked.Child("BRANCH", orderId / "CHILD") :: Nil)),
        attachingAllowed[Forked] orElse
        detachingAllowed[Forked] orElse
        cancelationMarkingAllowed[Forked] orElse {
          case (_: OrderJoined, `detached` ) ⇒ _.isInstanceOf[Processed]
          case (_: OrderBroken, _          ) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Offering" - {
      checkAllEvents(Order(orderId, workflowId, Offering(Timestamp("2018-11-19T12:00:00Z"))),
        cancelationMarkingAllowed[Offering] orElse {
          case (_: OrderBroken, _) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Awaiting" - {
      checkAllEvents(Order(orderId, workflowId, Awaiting(OrderId("OFFERED"))),
        cancelationMarkingAllowed[Awaiting] orElse {
          case (_: OrderJoined, `detached`) ⇒ _.isInstanceOf[Processed]
          case (_: OrderBroken, _         ) ⇒ _.isInstanceOf[Broken]
        })
    }

    "Finished" - {
      checkAllEvents(Order(orderId, workflowId, Finished), {
        case (_: OrderBroken, _) ⇒ _.isInstanceOf[Broken]
      })
    }

    type ToPredicate = PartialFunction[(OrderEvent, Option[AttachedState]), State ⇒ Boolean]

    def cancelationMarkingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderCancelationMarked, `detached` | `attaching` | `attached`) ⇒ implicitClass[S] isAssignableFrom _.getClass
    }

    def attachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderAttachable        , `detached` ) ⇒ implicitClass[S] isAssignableFrom _.getClass
      case (_: OrderTransferredToAgent, `attaching`) ⇒ implicitClass[S] isAssignableFrom _.getClass
    }

    def detachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (OrderDetachable         , `attached` ) ⇒ implicitClass[S] isAssignableFrom _.getClass
      case (OrderDetached           , `detaching`) ⇒ implicitClass[S] isAssignableFrom _.getClass
      case (OrderTransferredToMaster, `detaching`) ⇒ implicitClass[S] isAssignableFrom _.getClass
    }

    /** Checks each event in `allEvents`. */
    def checkAllEvents(order: Order[State], toPredicate: ToPredicate)
      (implicit pos: source.Position)
    : Unit =
      for (event ← allEvents) s"$event" - {
        for (a ← None :: attached :: detaching :: Nil) s"$a" in {
          val updated = order.copy(attachedState = a).update(event)
          val maybeState = updated.map(_.state)
          val maybePredicate = toPredicate.lift((event, a))
          (maybeState, maybePredicate) match {
            case (Valid(state), Some(predicate)) ⇒
              assert(predicate(state), s"- for  ${order.state} $a -> $event -> $state")
            case (Valid(state), None) ⇒
              fail(s"Missing test case for ${order.state} $a -> $event -> $state")
            case (Invalid(_), Some(_)) ⇒
              fail(s"Non-matching test case for ${order.state} $a -> $event -> ?")
            case (Invalid(_), None) ⇒
          }
        }
      }
  }

  "Operations" - {
    "attached" in {
      val agentId = AgentPath("/A") % "1"
      assert(testOrder.attached.isInvalid)
      assert(testOrder.copy(attachedState = Some(Attached(agentId))) .attached == Valid(agentId))
      assert(testOrder.copy(attachedState = Some(Detaching(agentId))).attached.isInvalid)
    }

    "detaching" in {
      val agentId = AgentPath("/A") % "1"
      assert(testOrder.detaching.isInvalid)
      assert(testOrder.copy(attachedState = Some(Attached(agentId))) .detaching.isInvalid)
      assert(testOrder.copy(attachedState = Some(Detaching(agentId))).detaching == Valid(agentId))
    }

    "castState" in {
      assert(testOrder.castState[Ready] eq testOrder)
      assert(testOrder.castState[FreshOrReady] eq testOrder)
      assert(testOrder.castState[State] eq testOrder)
      intercept[ProblemException] {
        testOrder.castState[Processed]
      }
    }

    "ifState" in {
      assert(testOrder.ifState[Ready] == Some(testOrder))
      assert(testOrder.ifState[FreshOrReady] == Some(testOrder))
      assert(testOrder.ifState[State] == Some(testOrder))
      assert(testOrder.ifState[Processed] == None)
    }

    "isAttaching" in {
      val order = Order(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW") % "VERSION", Ready,
        attachedState = Some(Detaching(AgentPath("/AGENT") % "1")))
      assert(order.detaching == Valid(AgentPath("/AGENT") % "1"))

      for (o ← Array(
            order.copy(attachedState = Some(Attached(AgentPath("/AGENT") % "1"))),
            order.copy(attachedState = None))) {
        val problem = o.detaching.asInstanceOf[Invalid[Problem]].e
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
      attachedState = Some(Attached(AgentPath("/AGENT") % "1")))
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
