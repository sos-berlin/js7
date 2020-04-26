package com.sos.jobscheduler.data.order

import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Order.{Attached, AttachedState, Attaching, Awaiting, Broken, Cancelled, DelayedAfterError, Detaching, Failed, FailedInFork, FailedWhileFresh, Finished, Forked, Fresh, IsFreshOrReady, Offering, Processed, Processing, Ready, State}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAwaiting, OrderAwoke, OrderBroken, OrderCancellationMarked, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderRetrying, OrderStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.Fork
import com.sos.jobscheduler.data.workflow.position.BranchId.Then
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.ListSet
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends AnyFreeSpec
{
  private val testOrder = Order(
    OrderId("ID"),
    WorkflowPath("/WORKFLOW") ~ "VERSION",
    Ready,
    arguments = Map(
      "key1" -> "value1",
      "key2" -> "value2"),
    HistoricOutcome(Position(123), Outcome.Succeeded(ReturnCode(0))) :: Nil)

  "JSON" - {
    "Order" - {
      "Ready" in {
        check(
          testOrder.copy(
            attachedState = Some(Attached(AgentRefPath("/AGENT"))),
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
            "arguments": {
              "key1": "value1",
              "key2": "value2"
            },
            "historicOutcomes": [
              {
                "position": [ 123 ],
                "outcome": {
                  "TYPE": "Succeeded",
                  "returnCode": 0
                }
              }
            ],
            "attachedState": {
              "TYPE": "Attached",
              "agentRefPath":"/AGENT"
            },
            "parent": "PARENT"
          }""")
      }

      "cancel" in {
        check(
          Order(OrderId("ID"), WorkflowPath("/WORKFLOW") ~ "VERSION", Fresh(), cancel = Some(CancelMode.NotStarted)),
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
            "historicOutcomes": [],
            "cancel": {
              "TYPE": "NotStarted"
            }
          }""")
      }

      def check(o: Order[State], json: Json) = testJson(o, json)
    }

    "State" - {
      "Fresh scheduled" in {
        testJson[State](Fresh(Some(Timestamp.parse("2017-11-15T12:33:44.789Z"))),
          json"""{
            "TYPE": "Fresh",
            "scheduledFor": 1510749224789
          }""")
      }

      "Fresh immediately" in {
        testJson[State](Fresh(),
          json"""{
            "TYPE": "Fresh"
          }""")
      }

      "Ready" in {
        testJson[State](Ready,
          json"""{
            "TYPE": "Ready"
          }""")
      }

      "Processing" in {
        testJson[State](Processing,
          json"""{
            "TYPE": "Processing"
          }""")
      }

      "Processed" in {
        testJson[State](Processed,
          json"""{
            "TYPE": "Processed"
          }""")
      }

      "FailedWhileFresh" in {
        testJson[State](FailedWhileFresh,
          json"""{
            "TYPE": "FailedWhileFresh"
          }""")
      }

      "DelayedAfterError" in {
        testJson[State](DelayedAfterError(Timestamp("2019-03-07T12:00:00Z")),
          json"""{
            "TYPE": "DelayedAfterError",
            "until": 1551960000000
          }""")
      }

      "Forked" in {
        testJson[State](Forked(List(
          Forked.Child(Fork.Branch.Id("A"), OrderId("A/1")),
          Forked.Child(Fork.Branch.Id("B"), OrderId("B/1")))),
          json"""{
            "TYPE": "Forked",
              "children": [
                {
                  "branchId": "A",
                  "orderId": "A/1"
                }, {
                  "branchId": "B",
                  "orderId": "B/1"
                }
              ]
            }""")
      }

      "Offering" in {
        testJson[State](Offering(Timestamp.ofEpochMilli(123)),
          json"""{
            "TYPE": "Offering",
            "until": 123
          }""")
      }

      "Finished" in {
        testJson[State](Finished,
          json"""{
            "TYPE": "Finished"
          }""")
      }

      "Broken" in {
        testJson[State](Broken(Problem("PROBLEM")),
          json"""{
            "TYPE": "Broken",
            "problem": {
              "message": "PROBLEM"
            }
          }""")
      }
    }

    "AttachedState" - {
      "Attached" in {
        testJson[AttachedState](Attached(AgentRefPath("/AGENT")),
          json"""{
            "TYPE": "Attached",
            "agentRefPath": "/AGENT"
          }""")
      }

      "Detaching" in {
        testJson[AttachedState](Detaching(AgentRefPath("/AGENT")),
          json"""{
            "TYPE": "Detaching",
            "agentRefPath": "/AGENT"
          }""")
      }
    }
  }

  "Order transitions: event to state" - {
    val orderId = OrderId("ID")
    val workflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
    val agentRefPath = AgentRefPath("/AGENT")
    val allEvents = ListSet[OrderCoreEvent](
      OrderAdded(workflowId),

      OrderAttachable(agentRefPath),
      OrderAttached(Map.empty, workflowId /: Position(0), Fresh(), Nil, None, agentRefPath),
      OrderTransferredToAgent(agentRefPath),

      OrderStarted,
      OrderProcessingStarted,
      //OrderStdoutWritten("stdout") is not an OrderCoreEvent
      //OrderStderrWritten("stderr") is not an OrderCoreEvent
      OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
      OrderFailed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(1)),
      OrderRetrying(Position(1)),
      OrderAwoke,
      OrderMoved(Position(1)),
      OrderForked(OrderForked.Child("BRANCH", orderId / "BRANCH") :: Nil),
      OrderJoined(Outcome.Succeeded(ReturnCode(0))),
      OrderOffered(OrderId("OFFERED"), until = Timestamp.ofEpochSecond(1)),
      OrderAwaiting(OrderId("OFFERED")),
      OrderFailed(Outcome.Failed(ReturnCode(1))),
      OrderFailedInFork(Outcome.Failed(ReturnCode(1))),
      OrderFinished,

      OrderCancellationMarked(CancelMode.NotStarted),
      OrderCancelled,
      OrderBroken(Problem("Problem")),

      OrderDetachable,
      OrderDetached,
      OrderTransferredToMaster
    )

    assert(allEvents.map(_.getClass) == OrderEvent.jsonCodec.classes[OrderCoreEvent])
    val detached  = none[AttachedState]
    val attaching = Some(Attaching(agentRefPath))
    val attached  = Some(Attached(agentRefPath))
    val detaching = Some(Detaching(agentRefPath))

    "Fresh" - {
      checkAllEvents(Order(orderId, workflowId, Fresh()),
        attachingAllowed[Fresh] orElse
        detachingAllowed[Fresh] orElse {
          case (_: OrderMoved            , `detached` | `attached`              ) => _.isInstanceOf[Fresh]
          case (_: OrderFailed           , `detached` | `attached`)               => _.isInstanceOf[FailedWhileFresh]  // Expression error
          case (_: OrderStarted          , `detached` | `attached`              ) => _.isInstanceOf[Ready]
          case (_: OrderCancellationMarked, `detached` | `attaching` | `attached`) => _.isInstanceOf[Fresh]
          case (OrderCancelled           , `detached`                           ) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken           , _                                    ) => _.isInstanceOf[Broken]
        })
    }

    "Ready" - {
      checkAllEvents(Order(orderId, workflowId, Ready),
        attachingAllowed[Ready] orElse
        detachingAllowed[Ready] orElse
        cancelationMarkingAllowed[Ready] orElse {
          case (_: OrderMoved            , `detached` | `attached`) => _.isInstanceOf[Ready]
          case (_: OrderProcessingStarted, `attached`             ) => _.isInstanceOf[Processing]
          case (_: OrderForked           , `detached` | `attached`) => _.isInstanceOf[Forked]
          case (_: OrderOffered          , `detached`             ) => _.isInstanceOf[Processed]
          case (_: OrderAwaiting         , `detached`             ) => _.isInstanceOf[Awaiting]
          case (_: OrderFailedInFork     , `detached` | `attached`) => _.isInstanceOf[FailedInFork]
          case (_: OrderCatched          , `detached` | `attached`) => _.isInstanceOf[Ready]
          case (_: OrderFailed           , `detached` | `attached`) => _.isInstanceOf[Failed]
          case (_: OrderRetrying         , `detached` | `attached`) => _.isInstanceOf[Ready]
          case (_: OrderFinished         , `detached`             ) => _.isInstanceOf[Finished]
          case (OrderCancelled           , `detached`             ) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken           , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "Processing" - {
      checkAllEvents(Order(orderId, workflowId, Processing),
        cancelationMarkingAllowed[Processing] orElse {
          case (_: OrderProcessed, `attached`) => _.isInstanceOf[Processed]
          case (_: OrderBroken   , _         ) => _.isInstanceOf[Broken]
      })
    }

    "Processed" - {
      checkAllEvents(Order(orderId, workflowId, Processed,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode(0))) :: Nil),
        cancelationMarkingAllowed[Processed] orElse {
          case (_: OrderMoved       , `detached` | `attached`) => _.isInstanceOf[Ready]
          case (_: OrderFailed      , `detached` | `attached`) => _.isInstanceOf[Failed]
          case (_: OrderFailedInFork, `detached` | `attached`) => _.isInstanceOf[FailedInFork]
          case (_: OrderCatched     , `detached` | `attached`) => _.isInstanceOf[Ready]
          case (_: OrderBroken      , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "FailedWhileFresh" - {
      checkAllEvents(Order(orderId, workflowId, FailedWhileFresh,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(ReturnCode(1))) :: Nil),
        detachingAllowed[FailedWhileFresh] orElse
        cancelationMarkingAllowed[FailedWhileFresh] orElse {
          case (OrderCancelled, `detached`) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _         ) => _.isInstanceOf[Broken]
        })
    }

    "Failed" - {
      checkAllEvents(Order(orderId, workflowId, Failed(Outcome.Failed(ReturnCode(1))),
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(ReturnCode(1))) :: Nil),
        detachingAllowed[Failed] orElse
        cancelationMarkingAllowed[Failed] orElse {
          case (OrderCancelled, `detached`) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _         ) => _.isInstanceOf[Broken]
        })
    }

    "FailedInFork" - {
      checkAllEvents(Order(orderId, workflowId, FailedInFork(Outcome.Failed(ReturnCode(1))),
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(ReturnCode(1))) :: Nil),
        detachingAllowed[FailedInFork])
    }

    "DelayedAfterError" - {
      checkAllEvents(Order(orderId, workflowId, DelayedAfterError(Timestamp("2019-03-07T12:00:00Z"))),
        cancelationMarkingAllowed[DelayedAfterError] orElse {
          case (OrderAwoke    , `attached`) => _.isInstanceOf[Order.Ready]
          case (OrderCancelled, `detached`) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _         ) => _.isInstanceOf[Broken]
        })
    }

    "Broken" - {
      checkAllEvents(Order(orderId, workflowId, Broken(Problem("PROBLEM"))),
        detachingAllowed[Broken] orElse
        cancelationMarkingAllowed[Broken] orElse {
          case (OrderCancelled, `detached`) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _         ) => _.isInstanceOf[Broken]
        })
    }

    "Forked" - {
      checkAllEvents(Order(orderId, workflowId, Forked(Forked.Child("BRANCH", orderId / "CHILD") :: Nil)),
        attachingAllowed[Forked] orElse
        detachingAllowed[Forked] orElse
        cancelationMarkingAllowed[Forked] orElse {
          case (_: OrderJoined, `detached` ) => _.isInstanceOf[Processed]
          case (_: OrderBroken, _          ) => _.isInstanceOf[Broken]
        })
    }

    "Offering" - {
      checkAllEvents(Order(orderId, workflowId, Offering(Timestamp("2018-11-19T12:00:00Z"))),
        cancelationMarkingAllowed[Offering] orElse {
          case (_: OrderBroken, _) => _.isInstanceOf[Broken]
        })
    }

    "Awaiting" - {
      checkAllEvents(Order(orderId, workflowId, Awaiting(OrderId("OFFERED"))),
        cancelationMarkingAllowed[Awaiting] orElse {
          case (_: OrderJoined, `detached`) => _.isInstanceOf[Processed]
          case (_: OrderBroken, _         ) => _.isInstanceOf[Broken]
        })
    }

    "Finished" - {
      checkAllEvents(Order(orderId, workflowId, Finished),
        PartialFunction.empty)
    }

    "attachedState" - {
      "attachedState=None" in {
        val order = Order(orderId, workflowId, Ready, attachedState = None)
        assert(order.update(OrderAttachable(agentRefPath)) == Right(order.copy(attachedState = Some(Attaching(agentRefPath)))))
        assert(order.update(OrderTransferredToAgent(agentRefPath)).isLeft)
        assert(order.update(OrderDetachable).isLeft)
        assert(order.update(OrderDetached).isLeft)
        assert(order.update(OrderTransferredToMaster).isLeft)
      }

      "attachedState=Attaching" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Attaching(agentRefPath)))
        assert(order.update(OrderAttachable(agentRefPath)).isLeft)
        assert(order.update(OrderTransferredToAgent(agentRefPath)) == Right(order.copy(attachedState = Some(Attached(agentRefPath)))))
        assert(order.update(OrderTransferredToAgent(AgentRefPath("/OTHER"))).isLeft)
        assert(order.update(OrderDetachable).isLeft)
        assert(order.update(OrderDetached).isLeft)
        assert(order.update(OrderTransferredToMaster).isLeft)
      }

      "attachedState=Attached" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Attached(agentRefPath)))
        assert(order.update(OrderAttachable(agentRefPath)).isLeft)
        assert(order.update(OrderTransferredToAgent(agentRefPath)).isLeft)
        assert(order.update(OrderTransferredToAgent(AgentRefPath("/OTHER"))).isLeft)
        assert(order.update(OrderDetachable) == Right(order.copy(attachedState = Some(Detaching(agentRefPath)))))
        assert(order.update(OrderDetached).isLeft)
        assert(order.update(OrderTransferredToMaster).isLeft)
      }

      "attachedState=Detaching" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Detaching(agentRefPath)))
        assert(order.update(OrderAttachable(agentRefPath)).isLeft)
        assert(order.update(OrderTransferredToAgent(agentRefPath)).isLeft)
        assert(order.update(OrderTransferredToAgent(AgentRefPath("/OTHER"))).isLeft)
        assert(order.update(OrderDetachable).isLeft)
        assert(order.update(OrderDetached) == Right(order.copy(attachedState = None)))
        assert(order.update(OrderTransferredToMaster) == Right(order.copy(attachedState = None)))
      }
    }

    type ToPredicate = PartialFunction[(OrderEvent, Option[AttachedState]), State => Boolean]

    def cancelationMarkingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderCancellationMarked, `detached` | `attaching` | `attached`) => implicitClass[S] isAssignableFrom _.getClass
    }

    def attachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderAttachable        , `detached` ) => implicitClass[S] isAssignableFrom _.getClass
      case (_: OrderTransferredToAgent, `attaching`) => implicitClass[S] isAssignableFrom _.getClass
    }

    def detachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (OrderDetachable         , `attached` ) => implicitClass[S] isAssignableFrom _.getClass
      case (OrderDetached           , `detaching`) => implicitClass[S] isAssignableFrom _.getClass
      case (OrderTransferredToMaster, `detaching`) => implicitClass[S] isAssignableFrom _.getClass
    }

    /** Checks each event in `allEvents`. */
    def checkAllEvents(order: Order[State], toPredicate: ToPredicate)
      (implicit pos: source.Position)
    : Unit =
      for (event <- allEvents) s"$event" - {
        for (a <- None :: attached :: detaching :: Nil) s"${a getOrElse "Master"}" in {
          val updated = order.copy(attachedState = a).update(event)
          val maybeState = updated.map(_.state)
          val maybePredicate = toPredicate.lift((event, a))
          (maybeState, maybePredicate) match {
            case (Right(state), Some(predicate)) =>
              assert(predicate(state), s"- for  ${order.state} $a -> $event -> $state")
            case (Right(state), None) =>
              fail(s"Missing test case for ${order.state} $a -> $event -> $state")
            case (Left(problem), Some(_)) =>
              fail(s"Non-matching test case for ${order.state} $a -> $event -> ?  $problem")
            case (Left(_), None) =>
          }
        }
      }
  }

  "Operations" - {
    "attached" in {
      val agentRefPath = AgentRefPath("/A")
      assert(testOrder.attached.isLeft)
      assert(testOrder.copy(attachedState = Some(Attached(agentRefPath))) .attached == Right(agentRefPath))
      assert(testOrder.copy(attachedState = Some(Detaching(agentRefPath))).attached.isLeft)
    }

    "detaching" in {
      val agentRefPath = AgentRefPath("/A")
      assert(testOrder.detaching.isLeft)
      assert(testOrder.copy(attachedState = Some(Attached(agentRefPath))) .detaching.isLeft)
      assert(testOrder.copy(attachedState = Some(Detaching(agentRefPath))).detaching == Right(agentRefPath))
    }

    "castState" in {
      assert(testOrder.castState[Ready] eq testOrder)
      assert(testOrder.castState[IsFreshOrReady] eq testOrder)
      assert(testOrder.castState[State] eq testOrder)
      intercept[ProblemException] {
        testOrder.castState[Processed]
      }
    }

    "ifState" in {
      assert(testOrder.ifState[Ready] == Some(testOrder))
      assert(testOrder.ifState[IsFreshOrReady] == Some(testOrder))
      assert(testOrder.ifState[State] == Some(testOrder))
      assert(testOrder.ifState[Processed] == None)
    }

    "isAttaching" in {
      val order = Order(OrderId("ORDER-ID"), WorkflowPath("/WORKFLOW") ~ "VERSION", Ready,
        attachedState = Some(Detaching(AgentRefPath("/AGENT"))))
      assert(order.detaching == Right(AgentRefPath("/AGENT")))

      for (o <- Array(
            order.copy(attachedState = Some(Attached(AgentRefPath("/AGENT")))),
            order.copy(attachedState = None))) {
        val Left(problem) = o.detaching
        assert(problem.toString contains "ORDER-ID")
      }
    }
  }

  "forkPositionOf" in {
    assert(testOrder.withPosition(Position(1)).forkPosition.isLeft)
    assert(testOrder.withPosition(Position(1) / "fork+A" % 2).forkPosition == Right(Position(1)))
    assert(testOrder.withPosition(Position(1) / "fork+A" % 2 / Then % 3).forkPosition == Right(Position(1)))
    assert(testOrder.withPosition(Position(1) / "fork+A" % 2 / Then % 3 / "fork+B" % 4).forkPosition == Right(Position(1) / "fork+A" % 2 / Then % 3))
  }

  "Error message when updated failed" in {
    assert(testOrder.update(OrderDetachable) ==
      Left(Problem("Order 'ID' at position '/WORKFLOW~VERSION:0' in state 'Ready' (on Master) has received an inapplicable event: OrderDetachable")))
  }

  if (sys.props contains "test.speed") "Speed" in {
    val order = Order(OrderId("ORDER-1"), (WorkflowPath("/WORKFLOW") ~ "VERSION") /: Position(1), Ready,
      attachedState = Some(Attached(AgentRefPath("/AGENT"))))
    val json = (order: Order[State]).asJson
    testSpeed(100000, "asOrder")(json.as[Order[State]])
    def testSpeed(n: Int, ops: String)(what: => Unit): Unit = {
      val start = Timestamp.currentTimeMillis
      for (_ <- 1 to n) what
      val duration = Timestamp.currentTimeMillis - start
      println(s"${duration}ms/$n $ops ${(n * 1000L / duration).toString} $ops/s")
    }
  }
}
