package js7.data.order

import cats.syntax.option._
import io.circe.Json
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Problem, ProblemException}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
import js7.data.job.ReturnCode
import js7.data.order.Order.{Attached, AttachedState, Attaching, Awaiting, Broken, Cancelled, DelayedAfterError, Detaching, Failed, FailedInFork, FailedWhileFresh, Finished, Forked, Fresh, IsFreshOrReady, Offering, Processed, Processing, ProcessingCancelled, Ready, State}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAttachedToAgent, OrderAwaiting, OrderAwoke, OrderBroken, OrderCancelMarked, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingCancelled, OrderProcessingStarted, OrderResumeMarked, OrderResumed, OrderRetrying, OrderStarted, OrderSuspendMarked, OrderSuspended}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
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

      "mark" in {
        check(
          Order(OrderId("ID"), WorkflowPath("/WORKFLOW") ~ "VERSION", Fresh(),
            mark = Some(OrderMark.Cancelling(CancelMode.NotStarted)),
            isSuspended = true),
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
            "mark": {
              "TYPE": "Cancelling",
              "mode": {
                "TYPE": "NotStarted"
              }
            },
            "isSuspended": true
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

      "Cancelled" in {
        testJson[State](Cancelled,
          json"""{
            "TYPE": "Cancelled"
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
      OrderAttachedToAgent(workflowId /: Position(0), Fresh(), Map.empty, Nil, agentRefPath, None, None, false),
      OrderAttached(agentRefPath),

      OrderStarted,
      OrderProcessingStarted,
      //OrderStdoutWritten("stdout") is not an OrderCoreEvent
      //OrderStderrWritten("stderr") is not an OrderCoreEvent
      OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
      OrderProcessingCancelled,
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

      OrderCancelMarked(CancelMode.NotStarted),
      OrderCancelled,
      OrderSuspendMarked,
      OrderSuspended,
      OrderResumeMarked,
      OrderResumed,

      OrderBroken(Problem("Problem")),

      OrderDetachable,
      OrderDetached,
      OrderDetached
    )

    "Event list is complete" in {
      assert(allEvents.map(_.getClass) == OrderEvent.jsonCodec.classes[OrderCoreEvent])
    }

    val IsDetached  = none[AttachedState]
    val IsAttaching = Some(Attaching(agentRefPath))
    val IsAttached  = Some(Attached(agentRefPath))
    val IsDetaching = Some(Detaching(agentRefPath))

    val NoMark     = none[OrderMark]
    val Cancelling = OrderMark.Cancelling(CancelMode.FreshOrStarted()).some
    val Suspending = OrderMark.Suspending.some
    val Resuming   = OrderMark.Resuming.some

    case object IsSuspended {
      def unapply(order: Order[Order.State]) = Some(order.isSuspended)
    }

    "Fresh" - {
      checkAllEvents(Order(orderId, workflowId, Fresh()),
        markable[Fresh] orElse
        attachingAllowed[Fresh] orElse
        detachingAllowed[Fresh] orElse
        cancelMarkedAllowed[Fresh] orElse
        suspendMarkedAllowed[Fresh] orElse {
          case (_: OrderMoved    , _                 , IsDetached | IsAttached) => _.isInstanceOf[Fresh]
          case (_: OrderFailed   , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[FailedWhileFresh]  // Expression error
          case (_: OrderStarted  , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (OrderCancelled   , _                 , IsDetached             ) => _.isInstanceOf[Cancelled]
          case (OrderSuspended   , _                 , IsDetached             ) => _.isInstanceOf[Fresh]
          case (OrderResumeMarked, _                 , _                      ) => _.isInstanceOf[Fresh]
          case (OrderResumed     , IsSuspended(true) , IsDetached | IsAttached) => _.isInstanceOf[Fresh]
          case (_: OrderBroken   , _                 , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "Ready" - {
      checkAllEvents(Order(orderId, workflowId, Ready),
        markable[Ready] orElse
        attachingAllowed[Ready] orElse
        detachingAllowed[Ready] orElse
        cancelMarkedAllowed[Ready] orElse
        suspendMarkedAllowed[Ready] orElse {
          case (_: OrderMoved            , _                 , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderProcessingStarted, IsSuspended(false), IsAttached             ) => _.isInstanceOf[Processing]
          case (_: OrderForked           , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Forked]
          case (_: OrderOffered          , IsSuspended(false), IsDetached             ) => _.isInstanceOf[Processed]
          case (_: OrderAwaiting         , IsSuspended(false), IsDetached             ) => _.isInstanceOf[Awaiting]
          case (_: OrderFailedInFork     , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[FailedInFork]
          case (_: OrderCatched          , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderFailed           , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Failed]
          case (_: OrderRetrying         , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderFinished         , IsSuspended(false), IsDetached             ) => _.isInstanceOf[Finished]
          case (OrderCancelled           , _                 , IsDetached             ) => _.isInstanceOf[Cancelled]
          case (OrderSuspended           , _                 , IsDetached             ) => _.isInstanceOf[Ready]
          case (OrderResumeMarked        , _                 , _                      ) => _.isInstanceOf[Ready]
          case (OrderResumed             , IsSuspended(true) , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderBroken           , _                 , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "Processing" - {
      checkAllEvents(Order(orderId, workflowId, Processing),
        markable[Processing] orElse
        cancelMarkedAllowed[Processing] orElse
        suspendMarkedAllowed[Processing] orElse {
          case (_: OrderProcessed, IsSuspended(false), IsAttached) => _.isInstanceOf[Processed]
          case (_: OrderBroken   , _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Processed" - {
      checkAllEvents(Order(orderId, workflowId, Processed,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode(0))) :: Nil),
        markable[Processed] orElse
        cancelMarkedAllowed[Processed] orElse
        suspendMarkedAllowed[Processed] orElse {
          case (_: OrderMoved              , _                 , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderProcessingCancelled, IsSuspended(false),              IsAttached) => _.isInstanceOf[ProcessingCancelled]
          case (_: OrderFailed             , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Failed]
          case (_: OrderFailedInFork       , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[FailedInFork]
          case (_: OrderCatched            , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderBroken             , _                 , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "ProcessingCancelled" - {
      checkAllEvents(Order(orderId, workflowId, ProcessingCancelled,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode(0))) :: Nil),
        markable[ProcessingCancelled] orElse
        detachingAllowed[ProcessingCancelled] orElse {
          case (OrderCancelled, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _, _         ) => _.isInstanceOf[Broken]
        })
    }

    "FailedWhileFresh" - {
      checkAllEvents(Order(orderId, workflowId, FailedWhileFresh,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(ReturnCode(1))) :: Nil),
        markable[FailedWhileFresh] orElse
        detachingAllowed[FailedWhileFresh] orElse
        cancelMarkedAllowed[FailedWhileFresh] orElse
        suspendMarkedAllowed[FailedWhileFresh] orElse {
          case (OrderCancelled, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _, _         ) => _.isInstanceOf[Broken]
        })
    }

    "Failed" - {
      checkAllEvents(Order(orderId, workflowId, Failed(Outcome.Failed(ReturnCode(1))),
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(ReturnCode(1))) :: Nil),
        markable[Failed] orElse
        detachingAllowed[Failed] orElse
        cancelMarkedAllowed[Failed] orElse {
          case (OrderCancelled, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _, _         ) => _.isInstanceOf[Broken]
        })
    }

    "FailedInFork" - {
      checkAllEvents(Order(orderId, workflowId, FailedInFork(Outcome.Failed(ReturnCode(1))),
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(ReturnCode(1))) :: Nil),
        markable[FailedInFork] orElse
        detachingAllowed[FailedInFork])
    }

    "DelayedAfterError" - {
      checkAllEvents(Order(orderId, workflowId, DelayedAfterError(Timestamp("2019-03-07T12:00:00Z"))),
        markable[DelayedAfterError] orElse
        cancelMarkedAllowed[DelayedAfterError] orElse
        suspendMarkedAllowed[DelayedAfterError] orElse {
          case (OrderAwoke    , IsSuspended(false), IsAttached) => _.isInstanceOf[Order.Ready]
          case (OrderCancelled, _                 , IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Broken" - {
      checkAllEvents(Order(orderId, workflowId, Broken(Problem("PROBLEM"))),
        markable[Broken] orElse
        detachingAllowed[Broken] orElse
        cancelMarkedAllowed[Broken] orElse {
          case (OrderCancelled   , _, IsDetached                           ) => _.isInstanceOf[Cancelled]
          case (OrderResumeMarked, _, IsDetached | IsAttached | IsDetaching) => _.isInstanceOf[Broken]
          case (OrderResumed     , _, IsDetached | IsAttached              ) => _.isInstanceOf[Ready]
          case (_: OrderBroken   , _, _                                    ) => _.isInstanceOf[Broken]
        })
    }

    "Forked" - {
      checkAllEvents(Order(orderId, workflowId, Forked(Forked.Child("BRANCH", orderId / "CHILD") :: Nil)),
        markable[Forked] orElse
        attachingAllowed[Forked] orElse
        detachingAllowed[Forked] orElse
        cancelMarkedAllowed[Forked] orElse
        suspendMarkedAllowed[Forked] orElse {
          case (_: OrderJoined, IsSuspended(false), IsDetached) => _.isInstanceOf[Processed]
          case (_: OrderBroken, _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Offering" - {
      checkAllEvents(Order(orderId, workflowId, Offering(Timestamp("2018-11-19T12:00:00Z"))),
        markable[Offering] orElse
        cancelMarkedAllowed[Offering] orElse
        suspendMarkedAllowed[Offering] orElse {
          case (_: OrderBroken, _, _) => _.isInstanceOf[Broken]
        })
    }

    "Awaiting" - {
      checkAllEvents(Order(orderId, workflowId, Awaiting(OrderId("OFFERED"))),
        markable[Awaiting] orElse
        cancelMarkedAllowed[Awaiting] orElse
        suspendMarkedAllowed[Awaiting] orElse {
          case (_: OrderJoined, IsSuspended(false), IsDetached) => _.isInstanceOf[Processed]
          case (_: OrderBroken, _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Cancelled" - {
      checkAllEvents(Order(orderId, workflowId, Cancelled),
        PartialFunction.empty)
    }

    "Finished" - {
      checkAllEvents(Order(orderId, workflowId, Finished),
        PartialFunction.empty)
    }

    "attachedState" - {
      "attachedState=None" in {
        val order = Order(orderId, workflowId, Ready, attachedState = None)
        assert(order.update(OrderAttachable(agentRefPath)) == Right(order.copy(attachedState = Some(Attaching(agentRefPath)))))
        assert(order.update(OrderAttached(agentRefPath)).isLeft)
        assert(order.update(OrderDetachable).isLeft)
        assert(order.update(OrderDetached).isLeft)
        assert(order.update(OrderDetached).isLeft)
      }

      "attachedState=Attaching" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Attaching(agentRefPath)))
        assert(order.update(OrderAttachable(agentRefPath)).isLeft)
        assert(order.update(OrderAttached(agentRefPath)) == Right(order.copy(attachedState = Some(Attached(agentRefPath)))))
        assert(order.update(OrderAttached(AgentRefPath("/OTHER"))).isLeft)
        assert(order.update(OrderDetachable).isLeft)
        assert(order.update(OrderDetached).isLeft)
        assert(order.update(OrderDetached).isLeft)
      }

      "attachedState=Attached" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Attached(agentRefPath)))
        assert(order.update(OrderAttachable(agentRefPath)).isLeft)
        assert(order.update(OrderAttached(agentRefPath)).isLeft)
        assert(order.update(OrderAttached(AgentRefPath("/OTHER"))).isLeft)
        assert(order.update(OrderDetachable) == Right(order.copy(attachedState = Some(Detaching(agentRefPath)))))
        assert(order.update(OrderDetached).isLeft)
        assert(order.update(OrderDetached).isLeft)
      }

      "attachedState=Detaching" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Detaching(agentRefPath)))
        assert(order.update(OrderAttachable(agentRefPath)).isLeft)
        assert(order.update(OrderAttached(agentRefPath)).isLeft)
        assert(order.update(OrderAttached(AgentRefPath("/OTHER"))).isLeft)
        assert(order.update(OrderDetachable).isLeft)
        assert(order.update(OrderDetached) == Right(order.copy(attachedState = None)))
        assert(order.update(OrderDetached) == Right(order.copy(attachedState = None)))
      }
    }

    type ToPredicate = PartialFunction[(OrderEvent, Order[Order.State], Option[AttachedState]), State => Boolean]

    def markable[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderCancelMarked | OrderSuspendMarked | OrderResumeMarked, _, _) => implicitClass[S] isAssignableFrom _.getClass
    }

    def cancelMarkedAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderCancelMarked, _, _) => implicitClass[S] isAssignableFrom _.getClass
    }

    def suspendMarkedAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (OrderSuspendMarked, IsSuspended(false)  , _) => implicitClass[S] isAssignableFrom _.getClass
      case (OrderResumeMarked , _, _) => implicitClass[S] isAssignableFrom _.getClass
    }

    def attachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderAttachable, _ , IsDetached) => implicitClass[S] isAssignableFrom _.getClass
      case (_: OrderAttached  , _, IsAttaching) => implicitClass[S] isAssignableFrom _.getClass
    }

    def detachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (OrderDetachable, _, IsAttached ) => implicitClass[S] isAssignableFrom _.getClass
      case (OrderDetached  , _, IsDetaching) => implicitClass[S] isAssignableFrom _.getClass
    }

    /** Checks each event in `allEvents`. */
    def checkAllEvents(templateOrder: Order[State], toPredicate: ToPredicate)(implicit pos: source.Position): Unit =
      for (event <- allEvents) s"$event" - {
        for (m <- Seq[Option[OrderMark]](NoMark, Cancelling, Suspending, Resuming)) s"$m" in {
          for (isSuspended <- Seq(false, true)) {
            for (a <- Seq(IsDetached, IsAttaching, IsAttached, IsDetaching)) /*SLOW (too many tests): s"${a getOrElse "Controller"}" -*/ {
              val aString = a match {
                case None => "detached"
                case Some(o) => o.getClass.simpleScalaName
              }
              val mString = a match {
                case None => "no mark"
                case Some(o) => o.getClass.simpleScalaName
              }
              val order = templateOrder.copy(attachedState = a, mark = m, isSuspended = isSuspended)
              val updated = order.update(event)
              val maybeState = updated.map(_.state)
              val maybePredicate = toPredicate.lift((event, order, a))
              (maybeState, maybePredicate) match {
                case (Right(state), Some(predicate)) =>
                  assert(predicate(state), s"- for  ${templateOrder.state} ($mString, isSuspended=$isSuspended, $aString) -> $event -> $state")
                case (Right(state), None) =>
                  fail(s"Missing test case for ${templateOrder.state} ($mString, isSuspended=$isSuspended, $aString) -> $event -> $state")
                case (Left(problem), Some(_)) =>
                  fail(s"Non-matching test case for ${templateOrder.state} ($mString, isSuspended=$isSuspended, $aString) -> $event -> ?  $problem")
                case (Left(_), None) =>
              }
            }
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
      Left(Problem("Order 'ID' at position '/WORKFLOW~VERSION:0' in state 'Ready' (on Controller) has received an inapplicable event: OrderDetachable")))
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
