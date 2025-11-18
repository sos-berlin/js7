package js7.data.order

import cats.syntax.option.*
import io.circe.Json
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, NoticeKey, NoticeV2_3}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.job.{InternalExecutable, JobKey}
import js7.data.lock.LockPath
import js7.data.order.Order.{Attached, AttachedState, Attaching, BetweenCycles, Broken, Cancelled, DelayedAfterError, DelayingRetry, Deleted, Detaching, ExpectingNotice, ExpectingNotices, ExternalOrderLink, Failed, FailedInFork, FailedWhileFresh, Finished, Forked, Fresh, InapplicableOrderEventProblem, IsFreshOrReady, Processed, Processing, ProcessingKilled, Prompting, Ready, Sleeping, State, Stopped, StoppedWhileFresh, WaitingForAdmission, WaitingForLock}
import js7.data.order.OrderEvent.{LegacyOrderLockEvent, LockDemand, OrderAdded, OrderAttachable, OrderAttached, OrderAttachedToAgent, OrderAwoke, OrderBroken, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCancelled, OrderCatched, OrderCaught, OrderCoreEvent, OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderDeleted, OrderDeletionMarked, OrderDetachable, OrderDetached, OrderExternalVanished, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderGoMarked, OrderGoes, OrderJoined, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased, OrderMoved, OrderNoticeAnnounced, OrderNoticeExpected, OrderNoticePosted, OrderNoticePostedV2_3, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead, OrderOrderAdded, OrderOutcomeAdded, OrderPlanAttached, OrderPriorityChanged, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderResumed, OrderResumptionMarked, OrderRetrying, OrderSaid, OrderSleeping, OrderStarted, OrderStateReset, OrderStickySubagentEntered, OrderStickySubagentLeaved, OrderStopped, OrderSuspended, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent, OrderTransferred, OrderWaitingForAdmission}
import js7.data.orderwatch.{ExternalOrderName, OrderWatchPath}
import js7.data.plan.{PlanId, PlanSchemaId}
import js7.data.subagent.{SubagentBundleId, SubagentId}
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ForkBranchId}
import js7.data.workflow.position.BranchId.{Then, catch_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalactic.source
import scala.annotation.nowarn
import scala.collection.View
import scala.collection.immutable.ListSet
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class OrderTest extends OurTestSuite:

  private val testOrder = Order(
    OrderId("ID"),
    WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0),
    Ready(),
    arguments = Map(
      "key1" -> StringValue("value1"),
      "key2" -> StringValue("value2")),
    historicOutcomes = Vector(
      HistoricOutcome(Position(123), OrderOutcome.Succeeded(NamedValues.rc(0)))))

  private val subagentId = SubagentId("SUBAGENT")

  "JSON" - {
    "Order" - {
      "Minimum" in:
        testJson[Order[Order.State]](
          Order(OrderId("ID"), (WorkflowPath("WORKFLOW") ~ "v1") /: Position(0),
            Order.Ready()),
          json"""{
            "id": "ID",
            "state": {
              "TYPE": "Ready"
            },
            "workflowPosition": {
              "position": [ 0 ],
              "workflowId": {
                "path": "WORKFLOW",
                "versionId": "v1"
              }
            }
          }""")

      "Ready" in:
        check(
          testOrder.copy(
            attachedState = Some(Attached(AgentPath("AGENT"))),
            parent = Some(OrderId("PARENT")),
            scheduledFor = Some(Timestamp.parse("2121-04-26T12:33:44.789Z")),
            priority = BigDecimal("1.23"),
            externalOrder = Some(ExternalOrderLink(
              OrderWatchPath("ORDER-WATCH"),
              ExternalOrderName("EXTERNAL"),
              vanished = true)),
            workflowPosition = WorkflowPath("WORKFLOW") ~ "VERSION" /: (Position(1) / "then" % 2),
            innerBlock = Position(1) / "then",
            stickySubagents = List(
              Order.StickySubagent(
                AgentPath("AGENT"),
                Some(SubagentBundleId("SUBAGENT-BUNDLE")))),
              forceAdmission = true),
          json"""{
            "id": "ID",
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
                "versionId": "VERSION"
              },
              "position": [ 1, "then", 2 ]
            },
            "innerBlock": [ 1, "then" ],
            "state": {
              "TYPE": "Ready"
            },
            "arguments": {
              "key1": "value1",
              "key2": "value2"
            },
            "scheduledFor": 4775114024789,
            "priority": 1.23,
            "externalOrder": {
              "orderWatchPath": "ORDER-WATCH",
              "name": "EXTERNAL",
              "vanished": true
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
            "parent": "PARENT",
            "stickySubagents": [{
              "agentPath": "AGENT",
              "subagentBundleId": "SUBAGENT-BUNDLE"
            }],
            "forceAdmission": true
          }""")

      "until v2.8.1" in:
        testJsonDecoder[Order[State]](
          Order(
            OrderId("ID"),
            WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0),
            Ready(),
            forceAdmission = true),
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
            "forceJobAdmission": true
          }""")

      "until v2.7.1" in:
        testJsonDecoder[Order[State]](
          Order(
            OrderId("ID"),
            WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0),
            Ready(),
            externalOrder = Some(Order.ExternalOrderLink(
              OrderWatchPath("ORDER-WATCH"),
              ExternalOrderName("EXTERNAL"))),
            stickySubagents = List(
              Order.StickySubagent(
                AgentPath("AGENT"),
                Some(SubagentBundleId("SUBAGENT-BUNDLE"))))),
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
            "externalOrderKey": {
              "orderWatchPath": "ORDER-WATCH",
              "name": "EXTERNAL"
            },
            "stickySubagents": [{
              "agentPath": "AGENT",
              "subagentSelectionId": "SUBAGENT-BUNDLE"
            }]
          }""")

      "Processing (extra Codec)" in:
        testJson[Order[Processing]](
          Order(
            OrderId("ID"),
            WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0),
            Processing(subagentId, endOfAdmissionPeriod = Some(ts"2025-07-03T12:00:00Z"))),
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
              "TYPE": "Processing",
              "subagentId": "SUBAGENT",
              "endOfAdmissionPeriod": 1751544000000
            }
          }""")

        testJson[Order[Processing]](
          Order(
            OrderId("ID"),
            WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0),
            Processing.legacy),
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
              "TYPE": "Processing"
            }
          }""")

      "mark" in:
        check(
          Order(OrderId("ID"), WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0), Fresh(),
            mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)),
            isSuspended = true,
            isResumed = true),
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
            "isSuspended": true,
            "isResumed": true
          }""")

      def check(o: Order[State], json: Json) = testJson(o, json)
    }

    "State" - {
      "Fresh immediately" in:
        testJson[State](Fresh(),
          json"""{
            "TYPE": "Fresh"
          }""")

      "Ready" in:
        testJson[State](Ready(),
          json"""{
            "TYPE": "Ready"
          }""")

      "Processing" in:
        testJson[State](Processing(SubagentId("SUBAGENT")),
          json"""{
            "TYPE": "Processing",
            "subagentId": "SUBAGENT"
          }""")

      "Processed" in:
        testJson[State](Processed,
          json"""{
            "TYPE": "Processed"
          }""")

      "FailedWhileFresh" in:
        testJson[State](FailedWhileFresh,
          json"""{
            "TYPE": "FailedWhileFresh"
          }""")

      "DelayingRetry" in:
        testJson[State](DelayingRetry(ts"2019-03-07T12:00:00Z"),
          json"""{
            "TYPE": "DelayingRetry",
            "until": 1551960000000
          }""")

      "DelayedAfterError" in:
        testJson[State](DelayedAfterError(ts"2019-03-07T12:00:00Z"),
          json"""{
            "TYPE": "DelayedAfterError",
            "until": 1551960000000
          }""")

      "Forked (distinct branches)" in:
        testJson[State](Forked(Vector(
          Forked.Child(ForkBranchId("A"), OrderId("A") / "1"),
          Forked.Child(ForkBranchId("B"), OrderId("B") / "2"))),
          json"""{
            "TYPE": "Forked",
              "children": [
                {
                  "orderId": "A|1",
                  "branchId": "A"
                }, {
                  "orderId": "B|2",
                  "branchId": "B"
                }
              ]
            }""")

      "Forked (ForkList) " in:
        testJson[State](Forked(Vector(
          Forked.Child(OrderId("A") / "1", Map("x" -> NumberValue(1))),
          Forked.Child(OrderId("B") / "2", Map.empty[String, Value]))),
          json"""{
            "TYPE": "Forked",
              "children": [
                {
                  "orderId": "A|1",
                  "arguments": {
                    "x": 1
                  }
                }, {
                  "orderId": "B|2"
                }
              ]
            }""")

      "WaitingForLock" in:
        testJson[State](WaitingForLock,
          json"""{
            "TYPE": "WaitingForLock"
          }""")

      "ExpectingNotice" in:
        testJson[State](ExpectingNotice(NoticeKey("NOTICE")),
          json"""{
            "TYPE": "ExpectingNotice",
            "noticeId": "NOTICE"
          }""")

      "ExpectingNotices" in:
        testJson[State](ExpectingNotices(Vector(
          PlanSchemaId("DailyPlan") / "2025-01-21" / BoardPath("BOARD") / NoticeKey("NOTICE"))),
          json"""{
            "TYPE": "ExpectingNotices",
            "noticeIds": [
              [ "DailyPlan", "2025-01-21", "BOARD", "NOTICE" ]
            ]
          }""")

        testJsonDecoder[State](ExpectingNotices(Vector(
          BoardPath("BOARD") \ "NOTICE")),
          json"""{
            "TYPE": "ExpectingNotices",
            "expected": [
              {
                "boardPath": "BOARD",
                "noticeId": "NOTICE"
              }
            ]
          }""")

      "Prompting" in:
        testJson[State](Prompting(StringValue("QUESTION")),
          json"""{
            "TYPE": "Prompting",
            "question": "QUESTION"
          }""")

      "BetweenCycles" in:
        testJson[State](
          BetweenCycles(Some(CycleState(
            end = ts"2021-10-01T00:00:00Z",
            schemeIndex = 1,
            periodIndex = 2,
            index = 3,
            next = ts"2021-10-01T12:00:00Z"))),
          json"""{
            "TYPE": "BetweenCycles",
            "cycleState": {
              "end": 1633046400000,
              "schemeIndex": 1,
              "periodIndex": 2,
              "index": 3,
              "next": 1633089600000
            }
          }""")

      "Sleeping" in:
        testJson[State](Sleeping(ts"2024-12-18T12:00:00Z"),
          json"""{
            "TYPE": "Sleeping",
            "until": 1734523200000
          }""")

      "WaitingForAdmission" in:
        testJson[State](WaitingForAdmission(ts"2024-12-18T12:00:00Z"),
          json"""{
            "TYPE": "WaitingForAdmission",
            "until": 1734523200000
          }""")

      "Cancelled" in:
        testJson[State](Cancelled,
          json"""{
            "TYPE": "Cancelled"
          }""")

      "Finished" in:
        testJson[State](Finished,
          json"""{
            "TYPE": "Finished"
          }""")

      "Broken" in:
        testJson[State](
          Broken(),
          json"""{
            "TYPE": "Broken"
          }""")

      "Broken until v2.4" in:
        testJson[State](
          Broken(Problem("PROBLEM")): @nowarn("msg=deprecated"),
          json"""{
            "TYPE": "Broken",
            "problem": {
              "message": "PROBLEM"
            }
          }""")
    }

    "AttachedState" - {
      "Attached" in:
        testJson[AttachedState](Attached(AgentPath("AGENT")),
          json"""{
            "TYPE": "Attached",
            "agentPath": "AGENT"
          }""")

      "Detaching" in:
        testJson[AttachedState](Detaching(AgentPath("AGENT")),
          json"""{
            "TYPE": "Detaching",
            "agentPath": "AGENT"
          }""")
    }
  }

  "priorityOrdering" in:
    val a = Order(OrderId("A"), WorkflowPath("WORKFLOW") /: Position(0), Fresh(), priority = 3)
    val b = Order(OrderId("B"), WorkflowPath("WORKFLOW") /: Position(0), Fresh(), priority = 2)
    val c = Order(OrderId("C"), WorkflowPath("WORKFLOW") /: Position(0), Fresh(), priority = 1)
    for permuation <- Seq(a, b, c).permutations do
      assert(permuation.sorted(using Order.priorityOrdering) == Seq(a, b, c))
    assert(Order.priorityOrdering.compare(a, b) == -1)
    assert(Order.priorityOrdering.compare(a, c) == -1)
    assert(Order.priorityOrdering.compare(b, c) == -1)
    assert(Order.priorityOrdering.compare(b, a) == +1)
    assert(Order.priorityOrdering.compare(c, a) == +1)
    assert(Order.priorityOrdering.compare(c, b) == +1)


  "Order transitions: event to state" - {
    val orderId = OrderId("ID")
    val workflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
    val agentPath = AgentPath("AGENT")
    val cycleState = CycleState.initial(TimeInterval(Timestamp.Epoch, 100*9000.h))
    val planId = PlanSchemaId("DailyPlan") / "2024-11-22"
    val allEvents = ListSet[OrderCoreEvent](
      OrderAdded(workflowId),
      OrderOrderAdded(OrderId("ADDED"), workflowId),
      OrderExternalVanished,
      OrderDeletionMarked,
      OrderDeleted,

      OrderAttachable(agentPath),
      OrderAttachedToAgent(workflowId /: Position(0), Fresh(), PlanId.Global, Map.empty, None, None,
        Vector.empty, agentPath, None, None, None, false, false),
      OrderAttached(agentPath),

      OrderStarted,
      OrderProcessingStarted(subagentId),
      //OrderStdoutWritten("stdout") is not an OrderCoreEvent
      //OrderStderrWritten("stderr") is not an OrderCoreEvent
      OrderProcessed(OrderOutcome.Succeeded(NamedValues.rc(0))),
      OrderProcessingKilled,
      OrderOutcomeAdded(OrderOutcome.Failed(NamedValues.rc(1))),
      OrderStopped,
      OrderFailed(Position(1)),
      OrderCatched(Position(1)),
      OrderCaught(Position(1)),
      OrderRetrying(),
      OrderAwoke,
      OrderMoved(Position(1)),
      OrderForked(Vector(OrderForked.Child("BRANCH", orderId / "BRANCH"))),
      OrderJoined(OrderOutcome.Succeeded(NamedValues.rc(0))),
      OrderFailedInFork(Position(1)),
      OrderFinished(),

      OrderCancellationMarked(CancellationMode.FreshOnly),
      OrderCancellationMarkedOnAgent,
      OrderCancelled,
      OrderStateReset,
      OrderSuspensionMarked(),
      OrderSuspensionMarkedOnAgent,
      OrderSuspended,
      OrderResumptionMarked(),
      OrderResumed(),

      OrderGoMarked(Position(0)),
      OrderGoes,

      OrderLocksAcquired(List(LockDemand(LockPath("LOCK")))),
      OrderLocksQueued(List(LockDemand(LockPath("LOCK"), None))),
      OrderLocksReleased(List(LockPath("LOCK"))),

      OrderPriorityChanged(7),

      OrderPlanAttached(planId),
      OrderNoticeAnnounced(BoardPath("BOARD") \ NoticeKey.empty),
      OrderNoticePostedV2_3(
        NoticeV2_3(NoticeKey("NOTICE"), endOfLife = Timestamp.ofEpochSecond(1))),
      OrderNoticePosted(
        BoardPath("BOARD") \ "NOTICE",
        endOfLife = Timestamp.ofEpochSecond(1).some),
      OrderNoticesExpected(Vector(
        BoardPath("BOARD") \ "NOTICE")),
      OrderNoticesRead,
      OrderNoticesConsumptionStarted(Vector.empty),
      OrderNoticesConsumed(),

      OrderPrompted(StringValue("QUESTION")),
      OrderPromptAnswered(),

      OrderCyclingPrepared(cycleState),
      OrderCycleStarted(),
      OrderCycleFinished(None),

      OrderStickySubagentLeaved,
      OrderStickySubagentEntered(agentPath),

      OrderSaid(StringValue("Said"): Value),
      OrderSleeping(ts"2024-12-18T00:00:00Z"),
      OrderWaitingForAdmission(ts"2024-12-18T00:00:00Z"),
      OrderTransferred(workflowId /: Position(0)),

      OrderBroken(),

      OrderDetachable,
      OrderDetached)

    "Event list is complete" in:
      assert(allEvents.map(_.getClass).toVector.sortBy(_.getName) ==
        OrderEvent.jsonCodec.classes[OrderCoreEvent]
          .filterNot(_ == classOf[OrderNoticeExpected])
          .filterNot(classOf[LegacyOrderLockEvent].isAssignableFrom)
          .toVector.sortBy(_.getName))

    val IsDetached  = none[AttachedState]
    val IsAttaching = Some(Attaching(agentPath))
    val IsAttached  = Some(Attached(agentPath))
    val IsDetaching = Some(Detaching(agentPath))

    val NoMark     = none[OrderMark]
    val Cancelling = OrderMark.Cancelling(CancellationMode.FreshOrStarted()).some
    val Suspending = OrderMark.Suspending().some
    val SuspendingWithKill = OrderMark.Suspending(SuspensionMode.kill).some
    val Resuming   = OrderMark.Resuming().some

    case object IsSuspended:
      def unapply(order: Order[Order.State]) = Some(order.isSuspended)

    case object IsSuspendingWithKill:
      def unapply(order: Order[Order.State]) = Some(order.isSuspendingWithKill)

    case object IsChild:
      def unapply(order: Order[Order.State]) = Some(order.parent.nonEmpty)

    "Fresh" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Fresh()),
        markable[Fresh] orElse
        attachingAllowed[Fresh] orElse
        detachingAllowed[Fresh] orElse
        cancelMarkedAllowed[Fresh] orElse
        suspendMarkedAllowed[Fresh] orElse
        priorityChangedAllowed[Fresh] orElse
        planAttached[Fresh] orElse:
          case (_: OrderNoticeAnnounced, _              , _, IsDetached             ) => _.isInstanceOf[Fresh]
          case (_: OrderMoved       , _                 , _, IsDetached | IsAttached) => _.isInstanceOf[Fresh]
          case (_: OrderFailed      , IsSuspended(false), _, IsDetached             ) => _.isInstanceOf[FailedWhileFresh]  // Expression error
          case (_: OrderStopped     , IsSuspended(false), _, IsDetached             ) => _.isInstanceOf[StoppedWhileFresh]  // Expression error
          case (_: OrderStarted     , IsSuspended(false), _, IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderStickySubagentEntered, IsSuspended(false), _, IsDetached | IsAttached) => _.isInstanceOf[Fresh]
          case (OrderCancelled      , _                 , _, IsDetached             ) => _.isInstanceOf[Cancelled]
          case (OrderSuspended      , _                 , _, IsDetached             ) => _.isInstanceOf[Fresh]
          case (OrderSuspended      , IsSuspended(true) , _, IsAttached             ) => _.isInstanceOf[Fresh]
          case (_: OrderResumptionMarked, _             , _, _                      ) => _.isInstanceOf[Fresh]
          case (_: OrderResumed     , IsSuspended(true) , _, IsDetached | IsAttached) => _.isInstanceOf[Fresh]
          case (_: OrderOutcomeAdded, _                 , _, _                      ) => _.isInstanceOf[Fresh]
          case (_: OrderSaid        , _                 , _, IsAttached | IsDetached) => _.isInstanceOf[Fresh]
          case (_: OrderTransferred , _                 , _, IsDetached             ) => _.isInstanceOf[Fresh]
          case (_: OrderBroken      , _                 , _, _                      ) => _.isInstanceOf[Broken])

    "Ready" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Ready()),
        markable[Ready] orElse
        attachingAllowed[Ready] orElse
        detachingAllowed[Ready] orElse
        cancelMarkedAllowed[Ready] orElse
        suspendMarkedAllowed[Ready] orElse
        priorityChangedAllowed[Ready] orElse
        planAttached[Ready] orElse:
          case (_: OrderMoved            , _                 , _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderProcessingStarted, IsSuspended(false), _            , IsAttached             ) => _.isInstanceOf[Processing]
          case (_: OrderForked           , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Forked]
          case (_: OrderFailedInFork     , IsSuspended(false), IsChild(true), IsDetached | IsAttached) => _.isInstanceOf[FailedInFork]
          case (_: OrderCatched          , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderCaught           , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderFailed           , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Failed]
          case (_: OrderStopped          , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Stopped]
          case (_: OrderRetrying         , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderFinished         , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Finished]
          case (OrderCancelled           , _                 , _            , IsDetached             ) => _.isInstanceOf[Cancelled]
          case (OrderSuspended           , _                 , _            , IsDetached             ) => _.isInstanceOf[Ready]
          case (OrderSuspended           , IsSuspended(true) , _            , IsAttached             ) => _.isInstanceOf[Ready]
          case (_: OrderResumptionMarked , _                 , _            , _                      ) => _.isInstanceOf[Ready]
          case (_: OrderResumed          , IsSuspended(true) , _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderLocksAcquired    , _                 , _            , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderLocksQueued      , _                 , _            , IsDetached             ) => _.isInstanceOf[WaitingForLock]
          case (_: OrderNoticePostedV2_3 , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderNoticePosted     , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderNoticesExpected  , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[ExpectingNotices]
          case (_: OrderNoticesRead      , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderNoticesConsumptionStarted, IsSuspended(false), _    , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderPrompted         , _                 , _            , IsDetached             ) => _.isInstanceOf[Prompting]
          case (_: OrderCyclingPrepared  , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[BetweenCycles]
          case (_: OrderOrderAdded       , _                 , _            , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderStickySubagentEntered, IsSuspended(false), _        , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderOutcomeAdded     , _                 , _            , _                      ) => _.isInstanceOf[Ready]
          case (_: OrderSaid             , _                 , _            , IsAttached | IsDetached) => _.isInstanceOf[Ready]
          case (_: OrderSleeping         , _                 , _            , IsDetached | IsAttached) => _.isInstanceOf[Sleeping]
          case (_: OrderWaitingForAdmission, _               , _            , IsDetached | IsAttached) => _.isInstanceOf[WaitingForAdmission]
          case (_: OrderTransferred      , _                 , _            , IsDetached             ) => _.isInstanceOf[Ready]
          case (_: OrderBroken           , _                 , _            , _                      ) => _.isInstanceOf[Broken])

    "WaitingForLock" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), WaitingForLock),
        markable[WaitingForLock] orElse
        cancelMarkedAllowed[WaitingForLock] orElse
        suspendMarkedAllowed[WaitingForLock] orElse
        priorityChangedAllowed[WaitingForLock] orElse
        planAttached[WaitingForLock] orElse:
          case (_: OrderLocksAcquired, _, _, IsDetached) => _.isInstanceOf[Ready]
          case (_: OrderStateReset, _, _, _      ) => _.isInstanceOf[Ready]
          case (_: OrderCancelled    , _, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderOutcomeAdded , _, _, _         ) => _.isInstanceOf[WaitingForLock]
          case (_: OrderTransferred  , _, _, IsDetached) => _.isInstanceOf[WaitingForLock]
          case (_: OrderBroken       , _, _, _         ) => _.isInstanceOf[Broken])

    "ExpectingNotice" in:
      val expectingNotices = ExpectingNotices(Vector(
        BoardPath("BOARD") \ "NOTICE"))
      checkAllEvents(Order(orderId, workflowId /: Position(0), expectingNotices),
        markable[ExpectingNotices] orElse
        cancelMarkedAllowed[ExpectingNotices] orElse
        suspendMarkedAllowed[ExpectingNotices] orElse
        priorityChangedAllowed[ExpectingNotices] orElse:
          case (_: OrderNoticesRead, IsSuspended(false), _, IsDetached) => _.isInstanceOf[Ready]
          case (_: OrderNoticesConsumptionStarted, IsSuspended(false), _, IsDetached) => _.isInstanceOf[Ready]
          case (_: OrderCancelled   , _, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderOutcomeAdded, _, _, _         ) => _.isInstanceOf[ExpectingNotices]
          case (_: OrderStateReset  , _, _, _         ) => _.isInstanceOf[Ready]
          case (_: OrderTransferred , _, _, IsDetached) => _.isInstanceOf[ExpectingNotices]
          case (_: OrderBroken      , _, _, _         ) => _.isInstanceOf[Broken])

    "Processing" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Processing(subagentId)),
        markable[Processing] orElse
        cancelMarkedAllowed[Processing] orElse
        suspendMarkedAllowed[Processing] orElse
        priorityChangedAllowed[Processing] orElse
        planAttached[Processing] orElse:
          case (_: OrderProcessed, IsSuspended(false), _, IsAttached) => _.isInstanceOf[Processed]
          case (_: OrderOutcomeAdded, _              , _, _         ) => _.isInstanceOf[Processing]
          case (_: OrderTransferred , _              , _, IsDetached) => _.isInstanceOf[Processing] // Impossible combination
          case (_: OrderBroken   , _                 , _, _         ) => _.isInstanceOf[Broken])

    "Processed" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Processed,
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0))))),
        markable[Processed] orElse
        cancelMarkedAllowed[Processed] orElse
        suspendMarkedAllowed[Processed] orElse
        priorityChangedAllowed[Processed] orElse
        detachingAllowed[Processed] orElse
        planAttached[Processed] orElse:
          case (_: OrderMoved           , _                 , _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderProcessingKilled, IsSuspended(false), _            ,              IsAttached) => _.isInstanceOf[ProcessingKilled]
          case (_: OrderOutcomeAdded    , _                 , _            , _                      ) => _.isInstanceOf[Processed]
          case (_: OrderTransferred     , _                 , _            , IsDetached             ) => _.isInstanceOf[Processed]
          case (_: OrderFailed          , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Failed]
          case (_: OrderFailedInFork    , IsSuspended(false), IsChild(true), IsDetached | IsAttached) => _.isInstanceOf[FailedInFork]
          case (_: OrderCatched         , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderCaught          , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderStopped         , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Stopped]
          case (_: OrderBroken          , _                 , _            , _                      ) => _.isInstanceOf[Broken])

    "ProcessingKilled" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), ProcessingKilled,
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0))))),
        markable[ProcessingKilled] orElse
        priorityChangedAllowed[ProcessingKilled] orElse
        detachingAllowed[ProcessingKilled] orElse
        planAttached[ProcessingKilled] orElse:
          case (OrderCancelled, _                         , _, IsDetached) => _.isInstanceOf[Cancelled]
          case (OrderSuspended, IsSuspendingWithKill(true), _, IsDetached) => _.isInstanceOf[Ready]
          case (OrderSuspended, order                     , _, IsAttached)
            if order.isSuspendingWithKill && order.isSuspended => _.isInstanceOf[Ready]
          case (_: OrderFailedInFork, IsSuspended(false), IsChild(true), IsDetached | IsAttached) => _.isInstanceOf[FailedInFork]
          case (_: OrderOutcomeAdded, _                 , _            , _                      ) => _.isInstanceOf[ProcessingKilled]
          case (_: OrderTransferred , _                 , _            , IsDetached             ) => _.isInstanceOf[ProcessingKilled]
          case (_: OrderCatched     , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderCaught      , IsSuspended(false), _            , IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderFailed      , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Failed]
          case (_: OrderStopped     , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Stopped]
          case (_: OrderBroken      , _                 , _, _                                  ) => _.isInstanceOf[Broken])

    "Sleeping" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Sleeping(ts"2025-10-15T12:00:00Z"),
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0))))),
        cancelMarkedAllowed[Sleeping] orElse
        suspendMarkedAllowed[Sleeping] orElse
        priorityChangedAllowed[Sleeping] orElse
        detachingAllowed[Sleeping] orElse
        planAttached[Sleeping] orElse:
          case (_: OrderAwoke           , IsSuspended(false), _, IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderMoved           , _                 , _, IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderStateReset      , _                 , _, _                      ) => _.isInstanceOf[Ready]
          case (_: OrderOutcomeAdded    , _                 , _, _                      ) => _.isInstanceOf[Sleeping]
          case (_: OrderSuspensionMarked, IsSuspended(_)    , _, _                      ) => _.isInstanceOf[Sleeping]
          case (_: OrderResumptionMarked, IsSuspended(_)    , _, _                      ) => _.isInstanceOf[Sleeping]
          case (_: OrderGoMarked        , _                 , _, IsAttached             ) => _.isInstanceOf[Sleeping]
          case (_: OrderGoes            , _                 , _, _                      ) => _.isInstanceOf[Sleeping]
          case (_: OrderTransferred     , _                 , _, IsDetached             ) => _.isInstanceOf[Sleeping]
          case (_: OrderBroken          , _                 , _, _                      ) => _.isInstanceOf[Broken])

    "WaitingForAdmission" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), WaitingForAdmission(ts"2025-10-15T12:00:00Z"),
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0))))),
        cancelMarkedAllowed[WaitingForAdmission] orElse
        suspendMarkedAllowed[WaitingForAdmission] orElse
        priorityChangedAllowed[WaitingForAdmission] orElse
        planAttached[WaitingForAdmission] orElse:
          case (_: OrderMoved           , _             , _, IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderStateReset      , _             , _, _                      ) => _.isInstanceOf[Ready]
          case (_: OrderOutcomeAdded    , _             , _, _                      ) => _.isInstanceOf[WaitingForAdmission]
          case (_: OrderSuspensionMarked, IsSuspended(_), _, _                      ) => _.isInstanceOf[WaitingForAdmission]
          case (_: OrderWaitingForAdmission, _          , _, IsDetached | IsAttached) => _.isInstanceOf[WaitingForAdmission]
          case (_: OrderGoMarked        , _             , _, IsAttached             ) => _.isInstanceOf[WaitingForAdmission]
          case (_: OrderGoes            , _             , _, _                      ) => _.isInstanceOf[WaitingForAdmission]
          case (_: OrderTransferred     , _             , _, IsDetached             ) => _.isInstanceOf[WaitingForAdmission]
          case (OrderCancelled          , _             , _, IsDetached             ) => _.isInstanceOf[Cancelled]
          case (_: OrderBroken          , _             , _, _                      ) => _.isInstanceOf[Broken])

    "Prompting" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Prompting(StringValue("QUESTION")),
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0))))),
        markable[Prompting] orElse
        cancelMarkedAllowed[Prompting] orElse
        suspendMarkedAllowed[Prompting] orElse
        priorityChangedAllowed[Prompting] orElse
        planAttached[Prompting] orElse:
          case (_: OrderPromptAnswered, _, _, IsDetached) => _.isInstanceOf[Ready]
          case (OrderCancelled        , _, _, IsDetached) => _.isInstanceOf[Cancelled]  // COMPATIBLE with v2.4
          case (OrderStateReset       , _, _,          _) => _.isInstanceOf[Ready]
          case (_: OrderOutcomeAdded  , _, _, _         ) => _.isInstanceOf[Prompting]
          case (_: OrderTransferred   , _, _, IsDetached) => _.isInstanceOf[Prompting]
          case (_: OrderBroken        , _, _, _         ) => _.isInstanceOf[Broken])

    "BetweenCycles" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), BetweenCycles(Some(cycleState)),
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0))))),
        markable[BetweenCycles] orElse
        cancelMarkedAllowed[BetweenCycles] orElse
        suspendMarkedAllowed[BetweenCycles] orElse
        priorityChangedAllowed[BetweenCycles] orElse
        detachingAllowed[BetweenCycles] orElse
        planAttached[BetweenCycles] orElse:
          case (_: OrderCyclingPrepared, IsSuspended(false), _, IsDetached | IsAttached) => _.isInstanceOf[BetweenCycles]
          case (_: OrderCycleStarted   , IsSuspended(false), _, IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (_: OrderMoved          , _                 , _, IsDetached | IsAttached) => _.isInstanceOf[Ready]
          case (OrderCancelled         , _                 , _, IsDetached             ) => _.isInstanceOf[Cancelled]
          case (_: OrderOutcomeAdded   , _                 , _, _                      ) => _.isInstanceOf[BetweenCycles]
          case (_: OrderTransferred    , _                 , _, IsDetached             ) => _.isInstanceOf[BetweenCycles]
          case (OrderStateReset        , _                 , _, _                      ) => _.isInstanceOf[Ready]
          case (_: OrderGoMarked       , _                 , _, IsAttached             ) => _.isInstanceOf[BetweenCycles]
          case (_: OrderGoes           , _                 , _, IsDetached | IsAttached) => _.isInstanceOf[BetweenCycles]
          case (_: OrderFailed         , _                 , _, IsDetached             ) => _.isInstanceOf[Failed]
          case (_: OrderBroken         , _                 , _, _                      ) => _.isInstanceOf[Broken])


    "FailedWhileFresh" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), FailedWhileFresh,
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Failed(NamedValues.rc(1))))),
        markable[FailedWhileFresh] orElse
        cancelMarkedAllowed[FailedWhileFresh] orElse
        suspendMarkedAllowed[FailedWhileFresh] orElse
        priorityChangedAllowed[FailedWhileFresh] orElse
        planAttached[FailedWhileFresh] orElse:
          case (OrderCancelled      , _, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderOutcomeAdded, _, _, _         ) => _.isInstanceOf[FailedWhileFresh]
          case (_: OrderTransferred , _, _, IsDetached) => _.isInstanceOf[FailedWhileFresh]
          case (_: OrderBroken      , _, _, _         ) => _.isInstanceOf[Broken])

    "Failed" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Failed,
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.failed))),
        markable[Failed] orElse
        priorityChangedAllowed[Failed] orElse
        cancelMarkedAllowed[Failed] orElse
        planAttached[Failed] orElse:
          case (_: OrderResumed     , IsSuspended(false), _, IsDetached) => _.isInstanceOf[Ready]
          case (OrderCancelled      , _, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (_: OrderOutcomeAdded, _, _, _         ) => _.isInstanceOf[Failed]
          case (_: OrderTransferred , _, _, IsDetached) => _.isInstanceOf[Failed]
          case (_: OrderBroken      , _, _, _         ) => _.isInstanceOf[Broken])

    "FailedInFork" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), FailedInFork, parent = Some(OrderId("PARENT")),
          historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Failed(NamedValues.rc(1))))),
        priorityChangedAllowed[FailedInFork] orElse
        cancelMarkedAllowed[FailedInFork] orElse
        planAttached[FailedInFork] orElse:
          case (_: OrderSuspensionMarked, IsSuspended(_), _, _         ) => _.isInstanceOf[FailedInFork]
          case (_: OrderResumptionMarked, IsSuspended(_), _, _         ) => _.isInstanceOf[FailedInFork]
          case (_: OrderOutcomeAdded    , _             , _, _         ) => _.isInstanceOf[FailedInFork]
          case (_: OrderTransferred     , _             , _, IsDetached) => _.isInstanceOf[FailedInFork]
          case (_: OrderBroken          , _             , _, _         ) => _.isInstanceOf[Broken])

    "DelayingRetry" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), DelayingRetry(ts"2024-09-26T12:00:00Z")),
        detachingAllowed[DelayingRetry] orElse
        markable[DelayingRetry] orElse
        cancelMarkedAllowed[DelayingRetry] orElse
        suspendMarkedAllowed[DelayingRetry] orElse
        priorityChangedAllowed[DelayingRetry] orElse
        planAttached[DelayingRetry] orElse:
          case (OrderAwoke    , IsSuspended(false), _, IsDetached | IsAttached) => _.isInstanceOf[Order.Ready]
          case (OrderCancelled, _                 , _, IsDetached             ) => _.isInstanceOf[Cancelled]
          case (_: OrderOutcomeAdded, _           , _, _                      ) => _.isInstanceOf[DelayingRetry]
          case (_: OrderTransferred , _           , _, IsDetached             ) => _.isInstanceOf[DelayingRetry]
          case (_: OrderGoMarked, _               , _, IsAttached             ) => _.isInstanceOf[DelayingRetry]
          case (_: OrderGoes, _                   , _, _                      ) => _.isInstanceOf[DelayingRetry]
          case (_: OrderStateReset, _             , _, _                      ) => _.isInstanceOf[Ready]
          case (_: OrderBroken, _                 , _, _                      ) => _.isInstanceOf[Broken])

    "DelayedAfterError" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), DelayedAfterError(ts"2019-03-07T12:00:00Z")),
        detachingAllowed[DelayedAfterError] orElse
        markable[DelayedAfterError] orElse
        cancelMarkedAllowed[DelayedAfterError] orElse
        suspendMarkedAllowed[DelayedAfterError] orElse
        priorityChangedAllowed[DelayedAfterError] orElse
        planAttached[DelayedAfterError] orElse:
          case (OrderAwoke    , IsSuspended(false), _, IsDetached | IsAttached) => _.isInstanceOf[Order.Ready]
          case (OrderCancelled, _                 , _, IsDetached             ) => _.isInstanceOf[Cancelled]
          case (_: OrderOutcomeAdded, _           , _, _                      ) => _.isInstanceOf[DelayedAfterError]
          case (_: OrderTransferred , _           , _, IsDetached             ) => _.isInstanceOf[DelayedAfterError]
          case (_: OrderGoMarked, _               , _, IsAttached             ) => _.isInstanceOf[DelayedAfterError]
          case (_: OrderGoes, _                   , _, _                      ) => _.isInstanceOf[DelayedAfterError]
          case (_: OrderBroken, _                 , _, _                      ) => _.isInstanceOf[Broken])

    "Broken" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Broken()),
        markable[Broken] orElse
        priorityChangedAllowed[Broken] orElse
        detachingAllowed[Broken] orElse
        cancelMarkedAllowed[Broken] orElse
        planAttached[Broken] orElse:
          case (OrderCancelled          , _, _, IsDetached                           ) => _.isInstanceOf[Cancelled]
          case (_: OrderResumptionMarked, _, _, IsDetached | IsAttached | IsDetaching) => _.isInstanceOf[Broken]
          case (_: OrderResumed         , _, _, IsDetached | IsAttached              ) => _.isInstanceOf[Ready]
          case (_: OrderOutcomeAdded    , _, _, _                                    ) => _.isInstanceOf[Broken]
          case (_: OrderTransferred     , _, _, IsDetached                           ) => _.isInstanceOf[Broken]
          case (_: OrderBroken          , _, _, _                                    ) => _.isInstanceOf[Broken]
          case (_: OrderFailed          , _, IsSuspended(false), IsDetached ) => _.isInstanceOf[Failed]
          case (_: OrderFailedInFork    , IsChild(true), IsSuspended(false), IsDetached | IsAttached) => _.isInstanceOf[FailedInFork]
          case (_: OrderStopped         , IsSuspended(false), _            , IsDetached             ) => _.isInstanceOf[Stopped]) // ???

    "Forked" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Forked(Vector(Forked.Child("BRANCH", orderId / "CHILD")))),
        markable[Forked] orElse
        attachingAllowed[Forked] orElse
        detachingAllowed[Forked] orElse
        cancelMarkedAllowed[Forked] orElse
        suspendMarkedAllowed[Forked] orElse
        priorityChangedAllowed[Forked] orElse
        planAttached[Forked] orElse:
          case (_: OrderJoined, IsSuspended(false), _, IsDetached) => _.isInstanceOf[Processed]
          case (_: OrderOutcomeAdded, _           , _, _         ) => _.isInstanceOf[Forked]
          case (_: OrderTransferred , _           , _, IsDetached) => _.isInstanceOf[Forked]
          case (_: OrderBroken, _                 , _, _         ) => _.isInstanceOf[Broken])

    "Cancelled" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Cancelled),
        planAttached[Cancelled] orElse:
          case (_: OrderOutcomeAdded, _, _, _         ) => _.isInstanceOf[Cancelled]
          case (_: OrderTransferred , _, _, IsDetached) => _.isInstanceOf[Cancelled]
          case (OrderDeleted, _, IsChild(false), IsDetached) => _.isInstanceOf[Deleted])

    "Finished" in:
      checkAllEvents(Order(orderId, workflowId /: Position(0), Finished),
        planAttached[Finished] orElse:
          case (_: OrderOutcomeAdded, _, _, _         ) => _.isInstanceOf[Finished]
          case (_: OrderTransferred , _, _, IsDetached) => _.isInstanceOf[Finished]
          case (OrderDeleted, _, IsChild(false), IsDetached) => _.isInstanceOf[Deleted])

    "attachedState" - {
      "attachedState=None" in:
        val order = Order(orderId, workflowId /: Position(0), Ready(), attachedState = None)
        assert(order.applyEvent(OrderAttachable(agentPath)) == Right(order.copy(attachedState = Some(Attaching(agentPath)))))
        assert(order.applyEvent(OrderAttached(agentPath)).isLeft)
        assert(order.applyEvent(OrderDetachable).isLeft)
        assert(order.applyEvent(OrderDetached).isLeft)

      "attachedState=Attaching" in:
        val order = Order(orderId, workflowId /: Position(0), Ready(), attachedState = Some(Attaching(agentPath)))
        assert(order.applyEvent(OrderAttachable(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(agentPath)) == Right(order.copy(attachedState = Some(Attached(agentPath)))))
        assert(order.applyEvent(OrderAttached(AgentPath("OTHER"))).isLeft)
        assert(order.applyEvent(OrderDetachable).isLeft)
        assert(order.applyEvent(OrderDetached) == Right(order.copy(attachedState = None)))

      "attachedState=Attached" in:
        val order = Order(orderId, workflowId /: Position(0), Ready(), attachedState = Some(Attached(agentPath)))
        assert(order.applyEvent(OrderAttachable(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(AgentPath("OTHER"))).isLeft)
        assert(order.applyEvent(OrderDetachable) == Right(order.copy(attachedState = Some(Detaching(agentPath)))))
        assert(order.applyEvent(OrderDetached) == Right(order.copy(attachedState = None)))

      "attachedState=Detaching" in:
        val order = Order(orderId, workflowId /: Position(0), Ready(), attachedState = Some(Detaching(agentPath)))
        assert(order.applyEvent(OrderAttachable(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(agentPath)).isLeft)
        assert(order.applyEvent(OrderAttached(AgentPath("OTHER"))).isLeft)
        assert(order.applyEvent(OrderDetachable).isLeft)
        assert(order.applyEvent(OrderDetached) == Right(order.copy(attachedState = None)))
    }

    type ToPredicate = PartialFunction[
      (OrderEvent, Order[Order.State], Order[Order.State], Option[AttachedState]),
      State => Boolean]

    def deletionMarkable[S <: Order.State: ClassTag]: ToPredicate =
      case (_: OrderDeletionMarked, _, IsChild(false), _) =>
        o => implicitClass[S].isAssignableFrom(o.getClass)

    def markable[S <: Order.State: ClassTag]: ToPredicate =
      case (_: OrderCancellationMarked | _: OrderSuspensionMarked | _: OrderResumptionMarked, _, _, _) =>
        o => implicitClass[S].isAssignableFrom(o.getClass)

    def cancelMarkedAllowed[S <: Order.State: ClassTag]: ToPredicate =
      case (_: OrderCancellationMarked, _, _, _) => o => implicitClass[S].isAssignableFrom(o.getClass)

    def suspendMarkedAllowed[S <: Order.State: ClassTag]: ToPredicate =
      case (_: OrderSuspensionMarked, IsSuspended(false), _, _) => o => implicitClass[S].isAssignableFrom(o.getClass)
      case (_: OrderResumptionMarked , _                , _, _) => o => implicitClass[S].isAssignableFrom(o.getClass)

    def priorityChangedAllowed[S <: Order.State: ClassTag]: ToPredicate =
      case (_: OrderPriorityChanged, _, _, _) => o => implicitClass[S].isAssignableFrom(o.getClass)

    def attachingAllowed[S <: Order.State: ClassTag]: ToPredicate =
      case (_: OrderAttachable, _, _, IsDetached ) => o => implicitClass[S].isAssignableFrom(o.getClass)
      case (_: OrderAttached  , _, _, IsAttaching) => o => implicitClass[S].isAssignableFrom(o.getClass)

    def detachingAllowed[S <: Order.State: ClassTag]: ToPredicate =
      case (OrderDetachable, _, _, IsAttached ) => o => implicitClass[S].isAssignableFrom(o.getClass)
      case (OrderDetached  , _, _, IsAttaching | IsAttached | IsDetaching) => o => implicitClass[S].isAssignableFrom(o.getClass)

    def planAttached[S <: Order.State: ClassTag]: ToPredicate =
      case (OrderPlanAttached(_), _, _, IsDetached) => o => implicitClass[S].isAssignableFrom(o.getClass)


    /** Checks each event in `allEvents`. */
    def checkAllEvents(templateOrder: Order[State], toPredicate: ToPredicate)(implicit pos: source.Position): Unit =
      allEvents foreach:
        // Some events are always allowed (even when attached):
        case OrderDeletionMarked =>
        case OrderExternalVanished =>
        case OrderCancellationMarkedOnAgent =>
        case OrderSuspensionMarkedOnAgent =>
        case event =>
          for
            m <- View[Option[OrderMark]](NoMark, Cancelling, Suspending, SuspendingWithKill, Resuming)
            isSuspended <- View(false, true)
            isChild <- View(false, true)
            a <- View(IsDetached, IsAttaching, IsAttached, IsDetaching) /*SLOW (too many tests): s"${a getOrElse "Controller"}" -*/
          do
            val mString = m.fold("no mark")(_.getClass.simpleScalaName)
            val aString = a.fold("detached")(_.getClass.simpleScalaName)
            val order = templateOrder.copy(attachedState = a, mark = m,
              parent = isChild ? OrderId("PARENT"),
              isSuspended = isSuspended)
            val updated = order.applyEvent(event)
            val maybeState = updated.map(_.state)
            val maybePredicate = toPredicate.lift((event, order, order, a))
            (maybeState, maybePredicate) match
              case (Right(state), Some(predicate)) =>
                assert(predicate(state), s"- for  ${templateOrder.state} state ($mString, isSuspended=$isSuspended, isChild=$isChild, $aString) <-: $event -> $state\n  $order")
              case (Right(state), None) =>
                fail(s"Missing test case for ${templateOrder.state} state ($mString, isSuspended=$isSuspended, isChild=$isChild, $aString) <-: $event -> $state\n  $order")
              case (Left(problem), Some(_)) =>
                fail(s"Failed test case for $order <-: $event -> 💥 $problem\n  $order")
              case (Left(_), None) =>
  }

  "Operations" - {
    "attached" in:
      val agentPath = AgentPath("A")
      assert(testOrder.attached.isLeft)
      assert(testOrder.copy(attachedState = Some(Attached(agentPath))) .attached == Right(agentPath))
      assert(testOrder.copy(attachedState = Some(Detaching(agentPath))).attached.isLeft)

    "detaching" in:
      val agentPath = AgentPath("A")
      assert(testOrder.detaching.isLeft)
      assert(testOrder.copy(attachedState = Some(Attached(agentPath))) .detaching.isLeft)
      assert(testOrder.copy(attachedState = Some(Detaching(agentPath))).detaching == Right(agentPath))

    "castState" in:
      assert(testOrder.castState[Ready] eq testOrder)
      assert(testOrder.castState[IsFreshOrReady] eq testOrder)
      assert(testOrder.castState[State] eq testOrder)
      intercept[ProblemException]:
        testOrder.castState[Processed]

    "ifState" in:
      assert(testOrder.ifState[Ready] == Some(testOrder))
      assert(testOrder.ifState[IsFreshOrReady] == Some(testOrder))
      assert(testOrder.ifState[State] == Some(testOrder))
      assert(testOrder.ifState[Processed] == None)

    "isAttaching" in:
      val order = Order(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0), Ready(),
        attachedState = Some(Detaching(AgentPath("AGENT"))))
      assert(order.detaching == Right(AgentPath("AGENT")))

      for (o <- Array(
            order.copy(attachedState = Some(Attached(AgentPath("AGENT")))),
            order.copy(attachedState = None)))
        val Left(problem) = o.detaching: @unchecked
        assert(problem.toString.contains("ORDER-ID"))

    "ifDelayed, isDelayed" in:
      val ts = ts"2025-09-10T12:00:00Z"
      val workflowPosition = WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0)

      val notDelayed = Order(OrderId("ORDER"), workflowPosition, Fresh())
      assert(!notDelayed.isDelayed(ts))
      assert(notDelayed.ifDelayed(ts) == None)

      val order = Order(OrderId("ORDER"), workflowPosition, Fresh(), scheduledFor = Some(ts))
      assert(order.isDelayed(now = ts - 1.s))
      assert(order.ifDelayed(now = ts - 1.s) == Some(ts))

      assert(!order.isDelayed(now = ts))
      assert(order.ifDelayed(now = ts) == None)

      assert(!order.isDelayed(now = ts + 1.s))
      assert(order.ifDelayed(now = ts + 1.s) == None)
  }

  "Events" - {
    "OrderResumed" - {
      import OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, HistoryOperation, InsertHistoricOutcome, ReplaceHistoricOutcome}

      lazy val order = Order(OrderId("ORDER-ID"), WorkflowPath("WORKFLOW") ~ "VERSION" /: Position(0), Ready(),
        historicOutcomes = Vector(
          HistoricOutcome(Position(0), OrderOutcome.succeeded),
          HistoricOutcome(Position(1), OrderOutcome.succeeded),
          HistoricOutcome(Position(2), OrderOutcome.succeeded)),
        isSuspended = true)

      "Truncate history at position" in:
        for i <- 0 to 2 do withClue(s"Position $i: "):
          assert(order.applyEvent(OrderResumed(Some(Position(i)), Nil)).toOption.get.historicOutcomes
            == order.historicOutcomes.take(i))

      def resume(operations: Seq[HistoryOperation]): Seq[HistoricOutcome] =
        order
          .applyEvent(OrderResumed(None, operations))
          .toOption.get.historicOutcomes

      "ReplaceHistoricOutcome" in:
        assert(resume(Seq(
          ReplaceHistoricOutcome(Position(1), OrderOutcome.failed))) ==
          Seq(
            HistoricOutcome(Position(0), OrderOutcome.succeeded),
            HistoricOutcome(Position(1), OrderOutcome.failed),
            HistoricOutcome(Position(2), OrderOutcome.succeeded)))

      "DeletedHistoricOutcome" in:
        assert(resume(Seq(
          DeleteHistoricOutcome(Position(1)))) ==
          Seq(
            HistoricOutcome(Position(0), OrderOutcome.succeeded),
            HistoricOutcome(Position(2), OrderOutcome.succeeded)))

      "InsertHistoricOutcome" in:
        assert(resume(Seq(
          InsertHistoricOutcome(Position(1), Position(1) / Then % 0, OrderOutcome.failed),
          InsertHistoricOutcome(Position(1), Position(1) / Then % 1, OrderOutcome.failed))) ==
          Seq(
            HistoricOutcome(Position(0), OrderOutcome.succeeded),
            HistoricOutcome(Position(1) / Then % 0, OrderOutcome.failed),
            HistoricOutcome(Position(1) / Then % 1, OrderOutcome.failed),
            HistoricOutcome(Position(1), OrderOutcome.succeeded),
            HistoricOutcome(Position(2), OrderOutcome.succeeded)))

      "AppendHistoricOutcome" in:
        assert(resume(Seq(
          AppendHistoricOutcome(Position(3), OrderOutcome.failed),
          AppendHistoricOutcome(Position(4), OrderOutcome.failed))) ==
          Seq(
            HistoricOutcome(Position(0), OrderOutcome.succeeded),
            HistoricOutcome(Position(1), OrderOutcome.succeeded),
            HistoricOutcome(Position(2), OrderOutcome.succeeded),
            HistoricOutcome(Position(3), OrderOutcome.failed),
            HistoricOutcome(Position(4), OrderOutcome.failed)))

      "Mixed" in:
        assert(resume(Seq(
          InsertHistoricOutcome(Position(2), Position(2) / Then % 0, OrderOutcome.failed),
          DeleteHistoricOutcome(Position(1)),
          DeleteHistoricOutcome(Position(2)))) ==
          Seq(
            HistoricOutcome(Position(0), OrderOutcome.succeeded),
            HistoricOutcome(Position(2) / Then % 0, OrderOutcome.failed)))
    }
  }

  "hasTimedOut" in:
    assert(!testOrder.copy(historicOutcomes = Vector.empty).hasTimedOut)
    assert(!testOrder.hasTimedOut)

    assert(!testOrder.applyEvent(OrderCaught(Position(1) / catch_(1) % 0 )).orThrow.hasTimedOut)

    var order = testOrder
      .applyEvent(OrderOutcomeAdded(OrderOutcome.TimedOut(OrderOutcome.succeeded))).orThrow
    assert(order.hasTimedOut)

    order = order.applyEvent(OrderCaught(Position(1) / catch_(1) % 0 )).orThrow
    assert(order.hasTimedOut)


  "forkPositionOf" in:
    assert(testOrder.withPosition(Position(1)).forkPosition.isLeft)
    assert(testOrder.withPosition(Position(1) / "fork+A" % 2).forkPosition == Right(Position(1)))
    assert(testOrder.withPosition(Position(1) / "fork+A" % 2 / Then % 3).forkPosition == Right(Position(1)))
    assert(testOrder.withPosition(Position(1) / "fork+A" % 2 / Then % 3 / "fork+B" % 4).forkPosition == Right(Position(1) / "fork+A" % 2 / Then % 3))

  "Error message when updated failed" in:
    assert(testOrder.applyEvent(OrderDetachable) ==
      Left(InapplicableOrderEventProblem(OrderDetachable, testOrder))) // "Order 'ID' at position 'WORKFLOW~VERSION:0' in state 'Ready', at Controller, received an inapplicable event: OrderDetachable")))

  "historicJobExecutionCount" in:
    val jobName = WorkflowJob.Name("JOB")
    val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1",
      Vector(
        Execute(WorkflowJob(AgentPath("AGENT"), InternalExecutable(classOf[OrderTest].getName))),
        Execute(jobName)),
      nameToJob = Map(
        jobName ->
          WorkflowJob(AgentPath("AGENT"), InternalExecutable(classOf[OrderTest].getName))))
    val order = testOrder.copy(historicOutcomes = Vector(
      HistoricOutcome(Position(0), OrderOutcome.succeeded),
      HistoricOutcome(Position(1), OrderOutcome.succeeded),
      HistoricOutcome(Position(0), OrderOutcome.succeeded),
      HistoricOutcome(Position(1), OrderOutcome.succeeded),
      HistoricOutcome(Position(0), OrderOutcome.succeeded)))

    assert(order.historicJobExecutionCount(JobKey(workflow.id /: Position(0)), workflow) == 3)
    assert(order.historicJobExecutionCount(JobKey(workflow.id /: Position(1)), workflow) == 0)
    assert(order.historicJobExecutionCount(JobKey(workflow.id, jobName), workflow) == 2)

  if sys.props contains "test.speed" then "Speed" in:
    val order = Order(OrderId("ORDER-1"), (WorkflowPath("WORKFLOW") ~ "VERSION") /: Position(1), Ready(),
      attachedState = Some(Attached(AgentPath("AGENT"))))
    val json = (order: Order[State]).asJson
    testSpeed(100000, "asOrder")(json.as[Order[State]])
    def testSpeed(n: Int, ops: String)(what: => Unit): Unit =
      val start = Timestamp.currentTimeMillis
      for _ <- 1 to n do what
      val duration = Timestamp.currentTimeMillis - start
      println(s"${duration}ms/$n $ops ${(n * 1000L / duration).toString} $ops/s")
