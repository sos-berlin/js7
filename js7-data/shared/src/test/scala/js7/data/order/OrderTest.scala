package js7.data.order

import cats.syntax.option._
import io.circe.Json
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Problem, ProblemException}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.command.{CancelMode, SuspendMode}
import js7.data.lock.LockPath
import js7.data.order.Order.{Attached, AttachedState, Attaching, Awaiting, Broken, Cancelled, DelayedAfterError, Detaching, Failed, FailedInFork, FailedWhileFresh, Finished, Forked, Fresh, InapplicableOrderEventProblem, IsFreshOrReady, Offering, Processed, Processing, ProcessingKilled, Ready, State, WaitingForLock}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAttachedToAgent, OrderAwaiting, OrderAwoke, OrderBroken, OrderCancelMarked, OrderCancelMarkedOnAgent, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderLockAcquired, OrderLockQueued, OrderLockReleased, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderRemoveMarked, OrderRemoved, OrderResumeMarked, OrderResumed, OrderRetrying, OrderStarted, OrderSuspendMarked, OrderSuspendMarkedOnAgent, OrderSuspended}
import js7.data.value.{NamedValues, StringValue}
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
    WorkflowPath("WORKFLOW") ~ "VERSION",
    Ready,
    arguments = Map(
      "key1" -> StringValue("value1"),
      "key2" -> StringValue("value2")),
    historicOutcomes = Seq(
      HistoricOutcome(Position(123), Outcome.Succeeded(NamedValues.rc(0)))))

  "JSON" - {
    "Order" - {
      "Ready" in {
        check(
          testOrder.copy(
            attachedState = Some(Attached(AgentPath("AGENT"))),
            parent = Some(OrderId("PARENT"))),
          json"""{
            "id": "ID",
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
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
                  "namedValues": {
                    "returnCode": 0
                  }
                }
              }
            ],
            "attachedState": {
              "TYPE": "Attached",
              "agentPath":"AGENT"
            },
            "parent": "PARENT"
          }""")
      }

      "mark" in {
        check(
          Order(OrderId("ID"), WorkflowPath("WORKFLOW") ~ "VERSION", Fresh(),
            mark = Some(OrderMark.Cancelling(CancelMode.FreshOnly)),
            isSuspended = true),
          json"""{
            "id": "ID",
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
                "versionId": "VERSION"
              },
              "position": [ 0 ]
            },
            "state": {
              "TYPE": "Fresh"
            },
            "mark": {
              "TYPE": "Cancelling",
              "mode": {
                "TYPE": "FreshOnly"
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

      "WaitingForLock" in {
        testJson[State](WaitingForLock,
          json"""{
            "TYPE": "WaitingForLock"
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
        testJson[AttachedState](Attached(AgentPath("AGENT")),
          json"""{
            "TYPE": "Attached",
            "agentPath": "AGENT"
          }""")
      }

      "Detaching" in {
        testJson[AttachedState](Detaching(AgentPath("AGENT")),
          json"""{
            "TYPE": "Detaching",
            "agentPath": "AGENT"
          }""")
      }
    }
  }

  "Order transitions: event to state" - {
    val orderId = OrderId("ID")
    val workflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
    val agentPath = AgentPath("AGENT")
    val allEvents = ListSet[OrderCoreEvent](
      OrderAdded(workflowId),
      OrderRemoveMarked,
      OrderRemoved,

      OrderAttachable(agentPath),
      OrderAttachedToAgent(workflowId /: Position(0), Fresh(), Map.empty, None, Nil, agentPath, None, None, false, false),
      OrderAttached(agentPath),

      OrderStarted,
      OrderProcessingStarted,
      //OrderStdoutWritten("stdout") is not an OrderCoreEvent
      //OrderStderrWritten("stderr") is not an OrderCoreEvent
      OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
      OrderProcessingKilled,
      OrderFailed(Position(1), Some(Outcome.Failed(NamedValues.rc(1)))),
      OrderCatched(Position(1)),
      OrderRetrying(Position(1)),
      OrderAwoke,
      OrderMoved(Position(1)),
      OrderForked(OrderForked.Child("BRANCH", orderId | "BRANCH") :: Nil),
      OrderJoined(Outcome.Succeeded(NamedValues.rc(0))),
      OrderOffered(OrderId("OFFERED"), until = Timestamp.ofEpochSecond(1)),
      OrderAwaiting(OrderId("OFFERED")),
      OrderFailed(Position(1), Some(Outcome.Failed(NamedValues.rc(1)))),
      OrderFailedInFork(Position(1), None),
      OrderFinished,

      OrderCancelMarked(CancelMode.FreshOnly),
      OrderCancelMarkedOnAgent,
      OrderCancelled,
      OrderSuspendMarked(),
      OrderSuspendMarkedOnAgent,
      OrderSuspended,
      OrderResumeMarked(),
      OrderResumed(),

      OrderLockAcquired(LockPath("LOCK")),
      OrderLockQueued(LockPath("LOCK"), None),
      OrderLockReleased(LockPath("LOCK")),

      OrderBroken(Problem("Problem")),

      OrderDetachable,
      OrderDetached,
      OrderDetached
    )

    "Event list is complete" in {
      assert(allEvents.map(_.getClass) == OrderEvent.jsonCodec.classes[OrderCoreEvent])
    }

    val IsDetached  = none[AttachedState]
    val IsAttaching = Some(Attaching(agentPath))
    val IsAttached  = Some(Attached(agentPath))
    val IsDetaching = Some(Detaching(agentPath))

    val NoMark     = none[OrderMark]
    val Cancelling = OrderMark.Cancelling(CancelMode.FreshOrStarted()).some
    val Suspending = OrderMark.Suspending().some
    val SuspendingWithKill = OrderMark.Suspending(SuspendMode(Some(CancelMode.Kill()))).some
    val Resuming   = OrderMark.Resuming().some

    case object IsSuspended {
      def unapply(order: Order[Order.State]) = Some(order.isSuspended)
    }

    case object IsSuspendingWithKill {
      def unapply(order: Order[Order.State]) = Some(order.isSuspendingWithKill)
    }

    "Fresh" in {
      checkAllEvents(Order(orderId, workflowId, Fresh()),
        removeMarkable[Fresh] orElse
        markable[Fresh] orElse
        attachingAllowed[Fresh] orElse
        detachingAllowed[Fresh] orElse
        cancelMarkedAllowed[Fresh] orElse
        suspendMarkedAllowed[Fresh] orElse {
          case (_: OrderMoved       , _                 , IsDetached | IsAttached) => _.isInstanceOf[Fresh]
          case (_: OrderFailed      , IsSuspended(false), IsDetached             ) => _.isInstanceOf[FailedWhileFresh]  // Expression error
          case (_: OrderStarted     , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (OrderCancelled      , _                 , IsDetached             ) => _.isInstanceOf[Cancelled]
          case (OrderSuspended      , _                 , IsDetached             ) => _.isInstanceOf[Fresh]
          case (OrderSuspended      , IsSuspended(true) , IsAttached             ) => _.isInstanceOf[Fresh]
          case (_: OrderResumeMarked, _                 , _                      ) => _.isInstanceOf[Fresh]
          case (_: OrderResumed     , IsSuspended(true) , IsDetached | IsAttached) => _.isInstanceOf[Fresh]
          case (_: OrderBroken      , _                 , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "Ready" in {
      checkAllEvents(Order(orderId, workflowId, Ready),
        removeMarkable[Ready] orElse
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
          case (_: OrderFailed           , IsSuspended(false), IsDetached             ) => _.isInstanceOf[Failed]
          case (_: OrderRetrying         , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderFinished         , IsSuspended(false), IsDetached             ) => _.isInstanceOf[Finished]
          case (OrderCancelled           , _                 , IsDetached             ) => _.isInstanceOf[Cancelled]
          case (OrderSuspended           , _                 , IsDetached             ) => _.isInstanceOf[Ready]
          case (OrderSuspended           , IsSuspended(true) , IsAttached             ) => _.isInstanceOf[Ready]
          case (_: OrderResumeMarked     , _                 , _                      ) => _.isInstanceOf[Ready]
          case (_: OrderResumed          , IsSuspended(true) , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderLockAcquired     , _                 , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderLockQueued       , _                 , IsDetached             ) => _.isInstanceOf[WaitingForLock]
          case (_: OrderBroken           , _                 , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "WaitingForLock" in {
      checkAllEvents(Order(orderId, workflowId, WaitingForLock),
        removeMarkable[WaitingForLock] orElse
        markable[WaitingForLock] orElse
        cancelMarkedAllowed[WaitingForLock] orElse
        suspendMarkedAllowed[WaitingForLock] orElse {
          case (_: OrderLockAcquired     , _                 , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderBroken           , _                 , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "Processing" in {
      checkAllEvents(Order(orderId, workflowId, Processing),
        removeMarkable[Processing] orElse
        markable[Processing] orElse
        cancelMarkedAllowed[Processing] orElse
        suspendMarkedAllowed[Processing] orElse {
          case (_: OrderProcessed, IsSuspended(false), IsAttached) => _.isInstanceOf[Processed]
          case (_: OrderBroken   , _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Processed" in {
      checkAllEvents(Order(orderId, workflowId, Processed,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(0))) :: Nil),
        removeMarkable[Processed] orElse
        markable[Processed] orElse
        cancelMarkedAllowed[Processed] orElse
        suspendMarkedAllowed[Processed] orElse
        detachingAllowed[Processed] orElse {
          case (_: OrderMoved           , _                 , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderProcessingKilled, IsSuspended(false),              IsAttached) => _.isInstanceOf[ProcessingKilled]
          case (_: OrderFailed          , IsSuspended(false), IsDetached             ) => _.isInstanceOf[Failed]
          case (_: OrderFailedInFork    , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[FailedInFork]
          case (_: OrderCatched         , IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderBroken          , _                 , _                      ) => _.isInstanceOf[Broken]
        })
    }

    "ProcessingKilled" in {
      checkAllEvents(Order(orderId, workflowId, ProcessingKilled,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(0))) :: Nil),
        removeMarkable[ProcessingKilled] orElse
        markable[ProcessingKilled] orElse
        detachingAllowed[ProcessingKilled] orElse {
          case (OrderCancelled, _                         , IsDetached) => _.isInstanceOf[Cancelled]
          case (OrderSuspended, IsSuspendingWithKill(true), IsDetached) => _.isInstanceOf[Ready]
          case (OrderSuspended, order, IsAttached) if order.isSuspendingWithKill && order.isSuspended => _.isInstanceOf[Ready]
          case (_: OrderBroken, _                         , _         ) => _.isInstanceOf[Broken]
        })
    }

    "FailedWhileFresh" in {
      checkAllEvents(Order(orderId, workflowId, FailedWhileFresh,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(NamedValues.rc(1))) :: Nil),
        removeMarkable[FailedWhileFresh] orElse
        markable[FailedWhileFresh] orElse
        detachingAllowed[FailedWhileFresh] orElse
        cancelMarkedAllowed[FailedWhileFresh] orElse
        suspendMarkedAllowed[FailedWhileFresh] orElse {
          case (OrderCancelled, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _, _         ) => _.isInstanceOf[Broken]
        })
    }

    "Failed" in {
      checkAllEvents(Order(orderId, workflowId, Failed,
          historicOutcomes = HistoricOutcome(Position(0), Outcome.failed) :: Nil),
        removeMarkable[Failed] orElse
        markable[Failed] orElse
        detachingAllowed[Failed] orElse
        cancelMarkedAllowed[Failed] orElse {
          case (OrderCancelled, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _, _         ) => _.isInstanceOf[Broken]
        })
    }

    "FailedInFork" in {
      checkAllEvents(Order(orderId, workflowId, FailedInFork, parent = Some(OrderId("PARENT")),
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(NamedValues.rc(1))) :: Nil),
        detachingAllowed[FailedInFork] orElse {
          case (_: OrderSuspendMarked, IsSuspended(_), _) => _.isInstanceOf[FailedInFork]
          case (_: OrderResumeMarked, IsSuspended(_), _) => _.isInstanceOf[FailedInFork]
        })
    }

    "DelayedAfterError" in {
      checkAllEvents(Order(orderId, workflowId, DelayedAfterError(Timestamp("2019-03-07T12:00:00Z"))),
        removeMarkable[DelayedAfterError] orElse
        markable[DelayedAfterError] orElse
        cancelMarkedAllowed[DelayedAfterError] orElse
        suspendMarkedAllowed[DelayedAfterError] orElse {
          case (OrderAwoke    , IsSuspended(false), IsAttached) => _.isInstanceOf[Order.Ready]
          case (OrderCancelled, _                 , IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken, _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Broken" in {
      checkAllEvents(Order(orderId, workflowId, Broken(Problem("PROBLEM"))),
        removeMarkable[Broken] orElse
        markable[Broken] orElse
        detachingAllowed[Broken] orElse
        cancelMarkedAllowed[Broken] orElse {
          case (OrderCancelled      , _, IsDetached                           ) => _.isInstanceOf[Cancelled]
          case (_: OrderResumeMarked, _, IsDetached | IsAttached | IsDetaching) => _.isInstanceOf[Broken]
          case (_: OrderResumed     , _, IsDetached | IsAttached              ) => _.isInstanceOf[Ready]
          case (_: OrderBroken      , _, _                                    ) => _.isInstanceOf[Broken]
        })
    }

    "Forked" in {
      checkAllEvents(Order(orderId, workflowId, Forked(Forked.Child("BRANCH", orderId | "CHILD") :: Nil)),
        removeMarkable[Forked] orElse
        markable[Forked] orElse
        attachingAllowed[Forked] orElse
        detachingAllowed[Forked] orElse
        cancelMarkedAllowed[Forked] orElse
        suspendMarkedAllowed[Forked] orElse {
          case (_: OrderJoined, IsSuspended(false), IsDetached) => _.isInstanceOf[Processed]
          case (_: OrderBroken, _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Offering" in {
      checkAllEvents(Order(orderId, workflowId, Offering(Timestamp("2018-11-19T12:00:00Z"))),
        removeMarkable[Offering] orElse
        markable[Offering] orElse
        cancelMarkedAllowed[Offering] orElse
        suspendMarkedAllowed[Offering] orElse {
          case (_: OrderBroken, _, _) => _.isInstanceOf[Broken]
        })
    }

    "Awaiting" in {
      checkAllEvents(Order(orderId, workflowId, Awaiting(OrderId("OFFERED"))),
        removeMarkable[Awaiting] orElse
        markable[Awaiting] orElse
        cancelMarkedAllowed[Awaiting] orElse
        suspendMarkedAllowed[Awaiting] orElse {
          case (_: OrderJoined, IsSuspended(false), IsDetached) => _.isInstanceOf[Processed]
          case (_: OrderBroken, _                 , _         ) => _.isInstanceOf[Broken]
        })
    }

    "Cancelled" in {
      checkAllEvents(Order(orderId, workflowId, Cancelled),
        removeMarkable[Cancelled] orElse {
          case (OrderRemoved, _, IsDetached) => _.isInstanceOf[Order.Removed]
        })
    }

    "Finished" in {
      checkAllEvents(Order(orderId, workflowId, Finished),
        removeMarkable[Finished] orElse {
          case (OrderRemoved, _, IsDetached) => _.isInstanceOf[Order.Removed]
        })
    }

    "attachedState" - {
      "attachedState=None" in {
        val order = Order(orderId, workflowId, Ready, attachedState = None)
        assert(order.applyEvent(OrderAttachable(agentPath)) == Right(order.copy(attachedState = Some(Attaching(agentPath)))))
        assert(order.applyEvent(OrderAttached(agentPath)).isLeft)
        assert(order.applyEvent(OrderDetachable).isLeft)
        assert(order.applyEvent(OrderDetached).isLeft)
        assert(order.applyEvent(OrderDetached).isLeft)
      }

      "attachedState=Attaching" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Attaching(agentPath)))
        assert(order.applyEvent(OrderAttachable(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(agentPath)) == Right(order.copy(attachedState = Some(Attached(agentPath)))))
        assert(order.applyEvent(OrderAttached(AgentPath("OTHER"))).isLeft)
        assert(order.applyEvent(OrderDetachable).isLeft)
        assert(order.applyEvent(OrderDetached).isLeft)
        assert(order.applyEvent(OrderDetached).isLeft)
      }

      "attachedState=Attached" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Attached(agentPath)))
        assert(order.applyEvent(OrderAttachable(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(AgentPath("OTHER"))).isLeft)
        assert(order.applyEvent(OrderDetachable) == Right(order.copy(attachedState = Some(Detaching(agentPath)))))
        assert(order.applyEvent(OrderDetached).isLeft)
        assert(order.applyEvent(OrderDetached).isLeft)
      }

      "attachedState=Detaching" in {
        val order = Order(orderId, workflowId, Ready, attachedState = Some(Detaching(agentPath)))
        assert(order.applyEvent(OrderAttachable(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(AgentPath("OTHER"))).isLeft)
        assert(order.applyEvent(OrderDetachable).isLeft)
        assert(order.applyEvent(OrderDetached) == Right(order.copy(attachedState = None)))
        assert(order.applyEvent(OrderDetached) == Right(order.copy(attachedState = None)))
      }
    }

    type ToPredicate = PartialFunction[(OrderEvent, Order[Order.State], Option[AttachedState]), State => Boolean]

    def removeMarkable[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderRemoveMarked, _, _) =>
        implicitClass[S] isAssignableFrom _.getClass
    }

    def markable[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderCancelMarked | _: OrderSuspendMarked | _: OrderResumeMarked, _, _) =>
        implicitClass[S] isAssignableFrom _.getClass
    }

    def cancelMarkedAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderCancelMarked, _, _) => implicitClass[S] isAssignableFrom _.getClass
    }

    def suspendMarkedAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderSuspendMarked, IsSuspended(false), _) => implicitClass[S] isAssignableFrom _.getClass
      case (_: OrderResumeMarked , _                 , _) => implicitClass[S] isAssignableFrom _.getClass
    }

    def attachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (_: OrderAttachable, _, IsDetached ) => implicitClass[S] isAssignableFrom _.getClass
      case (_: OrderAttached  , _, IsAttaching) => implicitClass[S] isAssignableFrom _.getClass
    }

    def detachingAllowed[S <: Order.State: ClassTag]: ToPredicate = {
      case (OrderDetachable, _, IsAttached ) => implicitClass[S] isAssignableFrom _.getClass
      case (OrderDetached  , _, IsDetaching) => implicitClass[S] isAssignableFrom _.getClass
    }

    /** Checks each event in `allEvents`. */
    def checkAllEvents(templateOrder: Order[State], toPredicate: ToPredicate)(implicit pos: source.Position): Unit =
      allEvents foreach {
        case OrderCancelMarkedOnAgent =>
        case OrderSuspendMarkedOnAgent =>
        case event =>
          for (m <- Seq[Option[OrderMark]](NoMark, Cancelling, Suspending, SuspendingWithKill, Resuming)) {
            for (isSuspended <- Seq(false, true)) {
              for (a <- Seq(IsDetached, IsAttaching, IsAttached, IsDetaching)) /*SLOW (too many tests): s"${a getOrElse "Controller"}" -*/ {
                val mString = m.fold("no mark")(_.getClass.simpleScalaName)
                val aString = a.fold("detached")(_.getClass.simpleScalaName)
                val order = templateOrder.copy(attachedState = a, mark = m, isSuspended = isSuspended)
                val updated = order.applyEvent(event)
                val maybeState = updated.map(_.state)
                val maybePredicate = toPredicate.lift((event, order, a))
                (maybeState, maybePredicate) match {
                  case (Right(state), Some(predicate)) =>
                    assert(predicate(state), s"- for  ${templateOrder.state} state ($mString, isSuspended=$isSuspended, $aString) -> $event -> $state\n  $order")
                  case (Right(state), None) =>
                    fail(s"Missing test case for ${templateOrder.state} state ($mString, isSuspended=$isSuspended, $aString) -> $event -> $state\n  $order")
                  case (Left(problem), Some(_)) =>
                    fail(s"Failed test case for ${templateOrder.state} state ($mString, isSuspended=$isSuspended, $aString) -> $event -> 💥 $problem\n  $order")
                  case (Left(_), None) =>
                }
              }
            }
          }
      }
  }

  "Operations" - {
    "attached" in {
      val agentPath = AgentPath("A")
      assert(testOrder.attached.isLeft)
      assert(testOrder.copy(attachedState = Some(Attached(agentPath))) .attached == Right(agentPath))
      assert(testOrder.copy(attachedState = Some(Detaching(agentPath))).attached.isLeft)
    }

    "detaching" in {
      val agentPath = AgentPath("A")
      assert(testOrder.detaching.isLeft)
      assert(testOrder.copy(attachedState = Some(Attached(agentPath))) .detaching.isLeft)
      assert(testOrder.copy(attachedState = Some(Detaching(agentPath))).detaching == Right(agentPath))
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
      val order = Order(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW") ~ "VERSION", Ready,
        attachedState = Some(Detaching(AgentPath("AGENT"))))
      assert(order.detaching == Right(AgentPath("AGENT")))

      for (o <- Array(
            order.copy(attachedState = Some(Attached(AgentPath("AGENT")))),
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
    assert(testOrder.applyEvent(OrderDetachable) ==
      Left(InapplicableOrderEventProblem(OrderDetachable, testOrder))) // "Order 'ID' at position 'WORKFLOW~VERSION:0' in state 'Ready', at Controller, received an inapplicable event: OrderDetachable")))
  }

  if (sys.props contains "test.speed") "Speed" in {
    val order = Order(OrderId("ORDER-1"), (WorkflowPath("WORKFLOW") ~ "VERSION") /: Position(1), Ready,
      attachedState = Some(Attached(AgentPath("AGENT"))))
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
