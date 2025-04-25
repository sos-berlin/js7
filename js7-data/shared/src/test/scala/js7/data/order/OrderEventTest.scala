package js7.data.order

import cats.syntax.option.*
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.time.TimestampForTests.ts
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, NoticeKey, NoticeV2_3}
import js7.data.command.CancellationMode
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.lock.LockPath
import js7.data.order.Order.ExternalOrderLink
import js7.data.order.OrderEvent.*
import js7.data.order.OrderEvent.OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, InsertHistoricOutcome, ReplaceHistoricOutcome}
import js7.data.orderwatch.{ExternalOrderKey, ExternalOrderName, OrderWatchPath}
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import js7.data.subagent.{SubagentBundleId, SubagentId}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Label, Position}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import scala.annotation.nowarn
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class OrderEventTest extends OurTestSuite:

  "OrderAdded" in:
    testJson[OrderEvent](
      OrderAdded(
        WorkflowPath("WORKFLOW") ~ "VERSION",
        Map("VAR" -> StringValue("VALUE")),
        PlanId(PlanSchemaId("DailyPlan"), PlanKey("2024-11-20")),
        Some(ts"2021-01-01T00:00:00Z"),
        priority = Order.DefaultPriority,
        Some(OrderWatchPath("ORDER-WATCH") / ExternalOrderName("ORDER-NAME")),
        deleteWhenTerminated = true,
        forceJobAdmission = true,
        innerBlock = Position(1) / "then",
        startPosition = Some(Position(1) / "then" % 2),
        stopPositions = Set(Position(1) / "then" % 9, Label("LABEL"))),
      json"""
      {
        "TYPE": "OrderAdded",
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "VERSION"
        },
        "scheduledFor": 1609459200000,
        "arguments": {
          "VAR": "VALUE"
        },
        "planId": [ "DailyPlan", "2024-11-20" ],
        "externalOrderKey": {
          "orderWatchPath": "ORDER-WATCH",
          "name": "ORDER-NAME"
        },
        "deleteWhenTerminated": true,
        "forceJobAdmission": true,
        "innerBlock": [ 1, "then" ],
        "startPosition": [ 1, "then", 2 ],
        "stopPositions": [ [ 1, "then", 9 ], "LABEL" ]
      }""")

    testJsonDecoder[OrderEvent](
      OrderAdded(WorkflowPath("WORKFLOW") ~ "VERSION"),
      json"""
      {
        "TYPE": "OrderAdded",
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "VERSION"
        }
      }""")

  "OrderOrderAdded" in:
    testJson[OrderEvent](
      OrderOrderAdded(
        OrderId("ORDER-ID"),
        WorkflowPath("WORKFLOW") ~ "VERSION",
        Map("VAR" -> StringValue("VALUE")),
        innerBlock = Position(1) / "then",
        startPosition = Some(Position(1) / "then" % 2),
        stopPositions = Set(Position(1) / "then" % 9, Label("LABEL")),
        deleteWhenTerminated = true,
        forceJobAdmission = true),
      json"""
      {
        "TYPE": "OrderOrderAdded",
        "orderId": "ORDER-ID",
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "VERSION"
        },
        "arguments": {
          "VAR": "VALUE"
        },
        "innerBlock": [ 1, "then" ],
        "startPosition": [ 1, "then", 2 ],
        "stopPositions": [ [ 1, "then", 9 ], "LABEL"],
        "deleteWhenTerminated": true,
        "forceJobAdmission": true
      }""")

    testJsonDecoder[OrderEvent](
      OrderOrderAdded(
        OrderId("ORDER-ID"),
        WorkflowPath("WORKFLOW") ~ "VERSION"),
      json"""
      {
        "TYPE": "OrderOrderAdded",
        "orderId": "ORDER-ID",
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "VERSION"
        }
      }""")

  "OrderAttachable" in:
    testJson[OrderEvent](
      OrderAttachable(AgentPath("AGENT")),
      json"""{
        "TYPE": "OrderAttachable",
        "agentPath": "AGENT"
      }""")

  "OrderAttachedToAgent" in:
    testJson[OrderEvent](
      OrderAttachedToAgent(
        (WorkflowPath("WORKFLOW") ~ "VERSION") /: Position(2),
        Order.Ready(),
        PlanId(PlanSchemaId("PLAN"), PlanKey("TODAY")),
        Map("KEY" -> StringValue("VALUE")),
        Some(Timestamp("2017-11-15T12:33:44.789Z")),
        Some(ExternalOrderLink(
          OrderWatchPath("ORDER-WATCH"),
          ExternalOrderName("ORDER-NAME"),
          vanished = true)),
        Vector(HistoricOutcome(Position(123), OrderOutcome.succeeded)),
        AgentPath("AGENT"),
        Some(OrderId("PARENT")),
        priority = Some(BigDecimal("1.23")),
        Some(OrderMark.Suspending()),
        isSuspended = true,
        isResumed = true,
        deleteWhenTerminated = true,
        forceJobAdmission = true,
        List(Order.StickySubagent(
          AgentPath("AGENT"),
          Some(SubagentBundleId("SUBAGENT-BUNDLE")))),
        innerBlock = Position(1) / "then",
        Set(Position(1) / "then" % 9, Label("LABEL"))),
      json"""{
        "TYPE": "OrderAttachedToAgent",
        "workflowPosition": {
          "workflowId": {
            "path": "WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 2 ]
        },
        "state": {
          "TYPE": "Ready"
        },
        "planId": [ "PLAN", "TODAY" ],
        "arguments": {
          "KEY": "VALUE"
        },
        "scheduledFor": 1510749224789,
        "priority": 1.23,
        "externalOrder": {
          "orderWatchPath": "ORDER-WATCH",
          "name": "ORDER-NAME",
          "vanished": true
        },
        "historicOutcomes": [
          {
            "position": [123],
            "outcome": {
              "TYPE": "Succeeded"
            }
          }
        ],
        "agentPath":"AGENT",
        "parent": "PARENT",
        "mark": {
          "TYPE": "Suspending",
          "mode": {
            "resetState": false
          }
        },
        "isSuspended": true,
        "isResumed": true,
        "deleteWhenTerminated": true,
        "forceJobAdmission": true,
        "stickySubagents": [{
          "agentPath": "AGENT",
          "subagentBundleId": "SUBAGENT-BUNDLE"
        }],
        "innerBlock": [ 1, "then" ],
        "stopPositions": [ [  1, "then", 9 ], "LABEL" ]
      }""")

    testJsonDecoder[OrderEvent](
      OrderAttachedToAgent(
        (WorkflowPath("WORKFLOW") ~ "VERSION") /: Position(2),
        Order.Ready(),
        externalOrder = Some(ExternalOrderLink(
          OrderWatchPath("ORDER-WATCH"),
          ExternalOrderName("ORDER-NAME"))),
        agentPath = AgentPath("AGENT"),
        stickySubagents = List(Order.StickySubagent(
          AgentPath("AGENT"),
          Some(SubagentBundleId("SUBAGENT-BUNDLE"))))),
      json"""
      {
        "TYPE": "OrderAttachedToAgent",
        "workflowPosition": {
          "workflowId": {
            "path": "WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 2 ]
        },
        "state": {
          "TYPE": "Ready"
        },
        "externalOrderKey": {
          "orderWatchPath": "ORDER-WATCH",
          "name": "ORDER-NAME"
        },
        "historicOutcomes": [],
        "agentPath":"AGENT",
        "stickySubagents": [{
          "agentPath": "AGENT",
          "subagentSelectionId": "SUBAGENT-BUNDLE"
        }]
      }""")

  "OrderAttached" in:
    testJson[OrderEvent](OrderAttached(AgentPath("AGENT")), json"""
      {
        "TYPE": "OrderAttached",
        "agentPath":"AGENT"
      }""")

  "OrderStarted" in:
    testJson[OrderEvent](OrderStarted, json"""
      {
        "TYPE": "OrderStarted"
      }""")

  "OrderProcessingStarted" in:
    testJson[OrderEvent](
      OrderProcessingStarted(
        Some(SubagentId("SUBAGENT")),
        Some(SubagentBundleId("BUNDLE")),
        stick = false),
      json"""
      {
        "TYPE": "OrderProcessingStarted",
        "subagentId": "SUBAGENT",
        "subagentBundleId": "BUNDLE",
        "stick": false
      }""")

    testJsonDecoder[OrderEvent](OrderProcessingStarted(SubagentId("SUBAGENT")), json"""
      {
        "TYPE": "OrderProcessingStarted",
        "subagentId": "SUBAGENT"
      }""")

    testJson[OrderEvent](OrderProcessingStarted.noSubagent, json"""
      {
        "TYPE": "OrderProcessingStarted",
        "stick": false
      }""")

  "OrderStdoutWritten toString" in:
    assert(OrderStderrWritten("*"*30 + "\r\n" + "*"*500).toString ==
      s"OrderStderrWritten(******************************␍⏎${"*" * 253}...(length 532))")

  "OrderStdoutWritten" in:
    testJson[OrderEvent](OrderStdoutWritten("STDOUT\n"), json"""
      {
        "TYPE": "OrderStdoutWritten",
        "chunk": "STDOUT\n"
      }""")

  "OrderStderrWritten" in:
    testJson[OrderEvent](OrderStderrWritten("STDOUT\n"), json"""
      {
        "TYPE": "OrderStderrWritten",
        "chunk": "STDOUT\n"
      }""")

  "OrderProcessed" in:
    testJson[OrderEvent](OrderProcessed(OrderOutcome.Succeeded(Map("KEY" -> StringValue("VALUE")))), json"""
      {
        "TYPE": "OrderProcessed",
        "outcome": {
          "TYPE": "Succeeded",
          "namedValues": {
            "KEY": "VALUE"
          }
        }
      }""")

  "OrderCatched" in:
    testJson[OrderEvent](OrderCatched(Position(1)), json"""
      {
        "TYPE": "OrderCatched",
        "movedTo": [ 1 ]
      }""")

  "OrderCatched complete" in:
    testJson[OrderEvent](OrderCatched(
      Position(1),
      Some(OrderOutcome.Failed(Some("FAILED"), NamedValues.rc(1)))),
      json"""
      {
        "TYPE": "OrderCatched",
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          },
          "message": "FAILED"
        },
        "movedTo": [ 1 ]
      }""")

  "OrderCaught" in:
    testJson[OrderEvent](OrderCaught(Position(1)), json"""
      {
        "TYPE": "OrderCaught",
        "movedTo": [ 1 ]
      }""")

  "OrderCaught complete" in:
    testJson[OrderEvent](
      OrderCaught(
        Position(1),
        Some(OrderOutcome.Failed(Some("FAILED"), NamedValues.rc(1)))
      ): @nowarn("msg=deprecated"),
      json"""
      {
        "TYPE": "OrderCaught",
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          },
          "message": "FAILED"
        },
        "movedTo": [ 1 ]
      }""")

  "OrderFailed" in:
    testJson[OrderEvent](OrderFailed(Position(1)),
      json"""
      {
        "TYPE": "OrderFailed",
        "movedTo": [ 1 ]
      }""")

  "OrderFailed(Failed) complete" in:
    testJson[OrderEvent](
      OrderFailed(
        Position(1),
        Some(OrderOutcome.Failed(Some("ERROR"), NamedValues.rc(1)))
      ): @nowarn("msg=deprecated"),
      json"""
      {
        "TYPE": "OrderFailed",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          },
          "message": "ERROR"
        }
      }""")

  "OrderFailed(Disrupted(PROBLEM))" in:
    testJson[OrderEvent](
      OrderFailed(
        Position(1),
        Some(OrderOutcome.Disrupted(Problem("PROBLEM")))
      ): @nowarn("msg=deprecated"),
      json"""{
        "TYPE": "OrderFailed",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "Other",
            "problem": {
              "message": "PROBLEM"
            }
          }
        }
      }""")

  "OrderFailedInFork" in:
    testJson[OrderEvent](OrderFailedInFork(Position(1)), json"""
      {
        "TYPE": "OrderFailedInFork",
        "movedTo": [ 1 ]
      }""")

  "OrderFailedInFork complete" in:
    testJson[OrderEvent](
      OrderFailedInFork(
        Position(1),
        Some(OrderOutcome.Failed(Some("ERROR"), NamedValues.rc(1)))
      ): @nowarn("msg=deprecated"),
      json"""{
        "TYPE": "OrderFailedInFork",
        "movedTo": [ 1 ],
        "outcome": {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          },
          "message": "ERROR"
        }
      }""")

  "OrderRetrying" in:
    testJson[OrderEvent](OrderRetrying(), json"""
      {
        "TYPE": "OrderRetrying"
      }""")

  "OrderRetrying(delayedUntil)" in:
    testJson[OrderEvent](OrderRetrying(Some(Timestamp("2019-03-04T12:00:00Z")), Some(Position(1))), json"""
      {
        "TYPE": "OrderRetrying",
        "movedTo": [ 1 ],
        "delayedUntil": 1551700800000
      }""")

  "OrderForked" in:
    testJson[OrderEvent](OrderForked(Vector(
      OrderForked.Child("A", OrderId("A/1")),
      OrderForked.Child("B", OrderId("B/1")))), json"""
      {
        "TYPE": "OrderForked",
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

  "OrderJoined" in:
    testJson[OrderEvent](OrderJoined(OrderOutcome.succeeded), json"""
      {
        "TYPE": "OrderJoined",
        "outcome": {
          "TYPE": "Succeeded"
        }
      }""")

  "OrderMoved" in:
    testJson[OrderEvent](OrderMoved(Position(7)), json"""
      {
        "TYPE": "OrderMoved",
        "to": [ 7 ]
      }""")

    testJson[OrderEvent](OrderMoved(Position(7), Some(OrderMoved.SkippedDueToWorkflowPathControl)), json"""
      {
        "TYPE": "OrderMoved",
        "to": [ 7 ],
        "reason": {
          "TYPE": "SkippedDueToWorkflowPathControl"
        }
      }""")

    testJson[OrderEvent](OrderMoved(Position(7), Some(OrderMoved.NoNotice)), json"""
      {
        "TYPE": "OrderMoved",
        "to": [ 7 ],
        "reason": {
          "TYPE": "NoNotice"
        }
      }""")

    testJson[OrderEvent](OrderMoved(Position(7), Some(OrderMoved.NoAdmissionPeriodStart)), json"""
      {
        "TYPE": "OrderMoved",
        "to": [ 7 ],
        "reason": {
          "TYPE": "NoAdmissionPeriodStart"
        }
      }""")

  "OrderCyclingPrepared" in:
    testJson[OrderEvent](OrderCyclingPrepared(CycleState.empty), json"""
      {
        "TYPE": "OrderCyclingPrepared",
        "cycleState": {
          "end": 0,
          "index": 0,
          "next": 0,
          "periodIndex": 0,
          "schemeIndex": 0
        }
      }""")

  "OrderCycleStarted" in:
    testJson[OrderEvent](OrderCycleStarted(), json"""
      {
        "TYPE": "OrderCycleStarted"
      }""")

    testJson[OrderEvent](OrderCycleStarted(Some(10.s)), json"""
      {
        "TYPE": "OrderCycleStarted",
        "skipped": 10
      }""")

  "OrderCycleFinished" in:
    testJson[OrderEvent](OrderCycleFinished(Some(CycleState.empty)), json"""
      {
        "TYPE": "OrderCycleFinished",
        "cycleState": {
          "end": 0,
          "index": 0,
          "next": 0,
          "periodIndex": 0,
          "schemeIndex": 0
        }
      }""")

  "OrderDetachable" in:
    testJson[OrderEvent](OrderDetachable, json"""
      {
        "TYPE": "OrderDetachable"
      }""")

  "OrderDetached" in:
    testJson[OrderEvent](OrderDetached, json"""
      {
        "TYPE": "OrderDetached"
      }""")

  "OrderFinished" in:
    testJson[OrderEvent](OrderFinished(), json"""
      {
        "TYPE": "OrderFinished"
      }""")

    testJson[OrderEvent](OrderFinished(Some(OrderOutcome.failed)), json"""
      {
        "TYPE": "OrderFinished",
        "outcome": {
          "TYPE": "Failed"
        }
      }""")

  "OrderCancellationMarked" in:
    testJson[OrderEvent](OrderCancellationMarked(CancellationMode.FreshOnly), json"""
      {
        "TYPE": "OrderCancellationMarked",
        "mode": {
          "TYPE": "FreshOnly"
        }
      }""")

    testJsonDecoder[OrderEvent](OrderCancellationMarked(), json"""
      {
        "TYPE": "OrderCancellationMarked"
      }""")

  "OrderCancellationMarkedOnAgent" in:
    testJson[OrderEvent](OrderCancellationMarkedOnAgent, json"""
      {
        "TYPE": "OrderCancellationMarkedOnAgent"
      }""")

  "OrderCancelled" in:
    testJson[OrderEvent](OrderCancelled, json"""
      {
        "TYPE": "OrderCancelled"
      }""")

  "OrderExternalVanished" in:
    testJson[OrderEvent](OrderExternalVanished, json"""
      {
        "TYPE": "OrderExternalVanished"
      }""")

  "OrderDeletionMarked" in:
    testJson[OrderEvent](OrderDeletionMarked, json"""
      {
        "TYPE": "OrderDeletionMarked"
      }""")

  "OrderDeleted" in:
    testJson[OrderEvent](OrderDeleted, json"""
      {
        "TYPE": "OrderDeleted"
      }""")

  "OrderSuspensionMarked" in:
    testJson[OrderEvent](OrderSuspensionMarked(), json"""
      {
        "TYPE": "OrderSuspensionMarked",
        "mode": {
          "resetState": false
        }
      }""")

    testJsonDecoder[OrderEvent](OrderSuspensionMarked(), json"""
      {
        "TYPE": "OrderSuspensionMarked"
      }""")

  "OrderSuspensionMarkedOnAgent" in:
    testJson[OrderEvent](OrderSuspensionMarkedOnAgent, json"""
      {
        "TYPE": "OrderSuspensionMarkedOnAgent"
      }""")

  "OrderSuspended" in:
    testJson[OrderEvent](OrderSuspended, json"""
      {
        "TYPE": "OrderSuspended"
      }""")

  "OrderGoMarked" in:
    testJson[OrderEvent](OrderGoMarked(Position(1)), json"""
      {
        "TYPE": "OrderGoMarked",
        "position": [ 1 ]
      }""")

  "OrderGoes" in:
    testJson[OrderEvent](OrderGoes, json"""
      {
        "TYPE": "OrderGoes"
      }""")

  "OrderResumptionMarked" in:
    testJson[OrderEvent](OrderResumptionMarked(
      Some(Position(1)),
      Seq(
        OrderResumed.ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded)),
      asSucceeded = true,
      restartKilledJob = true),
      json"""
        {
          "TYPE": "OrderResumptionMarked",
          "position": [ 1 ],
          "historyOperations": [
            {
              "TYPE": "Replace",
              "position": [ 0 ],
              "outcome": {
                "TYPE": "Succeeded"
              }
            }
          ],
          "asSucceeded": true,
          "restartKilledJob": true
        }""")

    testJsonDecoder[OrderEvent](OrderResumptionMarked(),
      json"""
        {
          "TYPE": "OrderResumptionMarked"
        }""")

  "OrderResumed" in:
    testJson[OrderEvent](OrderResumed(
      Some(Position(1)),
      Seq(
        ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded),
        InsertHistoricOutcome(Position(2), Position(1) / BranchId.Then % 0, OrderOutcome.succeeded),
        DeleteHistoricOutcome(Position(3)),
        AppendHistoricOutcome(Position(4), OrderOutcome.succeeded)),
      asSucceeded = true,
      restartKilledJob = true),
      json"""{
        "TYPE": "OrderResumed",
        "position": [ 1 ],
        "historyOperations": [
          {
            "TYPE": "Replace",
            "position": [ 0 ],
            "outcome": { "TYPE": "Succeeded" }
          }, {
            "TYPE": "Insert",
            "before": [ 2 ],
            "position": [ 1, "then", 0 ],
            "outcome": { "TYPE": "Succeeded" }
          }, {
            "TYPE": "Delete",
            "position": [ 3 ]
          },
          {
            "TYPE": "Append",
            "position": [ 4 ],
            "outcome": { "TYPE": "Succeeded" }
          }
        ],
        "asSucceeded": true,
        "restartKilledJob": true
      }""")

    testJsonDecoder[OrderEvent](OrderResumed(), json"""
      {
        "TYPE": "OrderResumed"
      }""")

  "OrderLocksAcquired" in:
    testJson[OrderEvent](OrderLocksAcquired(List(LockDemand(LockPath("LOCK"), Some(3)))), json"""
      {
        "TYPE": "OrderLocksAcquired",
        "demands": [{
          "lockPath": "LOCK",
          "count": 3
        }]
      }""")

    assert(
      json"""{
        "TYPE": "OrderLocksAcquired",
        "demands": [
          { "lockPath": "LOCK" },
          { "lockPath": "LOCK" }
        ]
      }""".checkedAs[OrderEvent] == Left(Problem(
        "JSON DecodingFailure at : Unexpected duplicates: 2×Lock:LOCK")))

    assert(
      json"""{
        "TYPE": "OrderLocksAcquired",
        "demands": [{
          "lockPath": "LOCK",
          "count": 0
        }]
      }""".checkedAs[OrderEvent] == Left(Problem(
        "JSON DecodingFailure at : LockDemand.count must not be below 1 for Lock:LOCK")))

  // COMPATIBLE with v2.4
  "OrderLockAcquired" in:
    testJsonDecoder[OrderEvent](OrderLocksAcquired(List(LockDemand(LockPath("LOCK"), Some(3)))),
      json"""
      {
        "TYPE": "OrderLockAcquired",
        "lockPath": "LOCK",
        "count": 3
      }""")

    assert(
      json"""{
        "TYPE": "OrderLockAcquired",
        "lockPath": "LOCK",
        "count": 0
      }""".checkedAs[OrderEvent] == Left(Problem(
        "JSON DecodingFailure at : LockDemand.count must not be below 1 for Lock:LOCK")))

  "OrderLocksQueued" in:
    testJson[OrderEvent](OrderLocksQueued(List(LockDemand(LockPath("LOCK"), Some(1)))), json"""
      {
        "TYPE": "OrderLocksQueued",
        "demands": [
          {
            "lockPath": "LOCK",
            "count": 1
          }
        ]
      }""")

    assert(
      json"""{
        "TYPE": "OrderLocksQueued",
        "demands": [
          { "lockPath": "LOCK" },
          { "lockPath": "LOCK" }
        ]
      }""".checkedAs[OrderEvent] == Left(Problem(
        "JSON DecodingFailure at : Unexpected duplicates: 2×Lock:LOCK")))

    assert(
      json"""{
        "TYPE": "OrderLocksQueued",
        "demands": [{
          "lockPath": "LOCK",
          "count": 0
        }]
      }""".checkedAs[OrderEvent] == Left(Problem(
        "JSON DecodingFailure at : LockDemand.count must not be below 1 for Lock:LOCK")))

  // COMPATIBLE with v2.4
  "OrderLockQueued" in:
    testJsonDecoder[OrderEvent](OrderLocksQueued(List(LockDemand(LockPath("LOCK"), Some(1)))),
      json"""
      {
        "TYPE": "OrderLockQueued",
        "lockPath": "LOCK",
        "count": 1
      }""")

    assert(
      json"""{
        "TYPE": "OrderLockQueued",
        "lockPath": "LOCK",
        "count": 0
      }""".checkedAs[OrderEvent] == Left(Problem(
        "JSON DecodingFailure at : LockDemand.count must not be below 1 for Lock:LOCK")))

  "OrderStateReset" in :
    testJson[OrderEvent](OrderStateReset,
      json"""
      {
        "TYPE": "OrderStateReset"
      }""")

  // COMPATIBLE with v2.7.1
  "OrderLocksDequeued" in:
    testJsonDecoder[OrderEvent](OrderStateReset, json"""
      {
        "TYPE": "OrderLocksDequeued",
        "lockPaths": [ "LOCK" ]
      }""")

  // COMPATIBLE with v2.4
  "OrderLockDequeued" in:
    testJsonDecoder[OrderEvent](OrderStateReset,
      json"""
      {
        "TYPE": "OrderLockDequeued",
        "lockPath": "LOCK"
      }""")

  "OrderLocksReleased" in:
    testJson[OrderEvent](OrderLocksReleased(List(LockPath("LOCK"))), json"""
      {
        "TYPE": "OrderLocksReleased",
        "lockPaths": [ "LOCK" ]
      }""")

    assert(
      json"""{
        "TYPE": "OrderLocksReleased",
        "lockPaths": [ "LOCK", "LOCK" ]
      }""".checkedAs[OrderEvent] == Left(Problem(
        "JSON DecodingFailure at : Unexpected duplicates: 2×Lock:LOCK")))

  // COMPATIBLE with v2.4
  "OrderLockReleased" in:
    testJsonDecoder[OrderEvent](OrderLocksReleased(List(LockPath("LOCK"))),
      json"""
      {
        "TYPE": "OrderLockReleased",
        "lockPath": "LOCK"
      }""")

  "OrderNoticeAnnounced" in :
    testJson[OrderEvent](
      OrderNoticeAnnounced(BoardPath("BOARD") \ "NOTICE"),
      json"""{
        "TYPE": "OrderNoticeAnnounced",
        "noticeId": [ "BOARD", "NOTICE" ]
      }""")

  "OrderNoticePosted until v2.3" in:
    testJson[OrderEvent](OrderNoticePostedV2_3(NoticeV2_3(
      NoticeKey("NOTICE"),
      endOfLife = Timestamp("1970-01-01T01:00:00Z"))),
      json"""
      {
        "TYPE": "OrderNoticePosted",
        "notice": {
          "id": "NOTICE",
          "endOfLife": 3600000
        }
      }""")

  "OrderNoticePosted" in:
    testJson[OrderEvent](
      OrderNoticePosted(
        BoardPath("BOARD") \ "NOTICE",
        endOfLife = Timestamp("1970-01-01T01:00:00Z").some),
      json"""
      {
        "TYPE": "OrderNoticePosted",
        "noticeId": [ "BOARD", "NOTICE" ],
        "endOfLife": 3600000
      }""")

    // COMPATIBLE with v2.7.3
    testJsonDecoder[OrderEvent](
      OrderNoticePosted(
        BoardPath("BOARD") \ "NOTICE",
        endOfLife = Timestamp("1970-01-01T01:00:00Z").some),
      json"""
      {
        "TYPE": "OrderNoticePosted",
        "notice": {
          "id": "NOTICE",
          "boardPath": "BOARD",
          "endOfLife": 3600000
        }
      }""")

  "OrderNoticeExpected" in:
    // COMPATIBLE with v2.3
    testJson[OrderEvent](OrderNoticeExpected(NoticeKey("NOTICE")),
      json"""
      {
        "TYPE": "OrderNoticeExpected",
        "noticeId": "NOTICE"
      }""")

  "OrderNoticesExpected" in:
    testJson[OrderEvent](OrderNoticesExpected(Vector(
      BoardPath("BOARD") \ "NOTICE")),
      json"""
      {
        "TYPE": "OrderNoticesExpected",
        "noticeIds": [
          [ "BOARD", "NOTICE" ]
        ]
      }""")

    // COMPATIBLE with v2.7.3
    testJsonDecoder[OrderEvent](OrderNoticesExpected(Vector(
      BoardPath("BOARD") \ "NOTICE")),
      json"""
      {
        "TYPE": "OrderNoticesExpected",
        "expected": [
          {
            "boardPath": "BOARD",
            "noticeId": "NOTICE"
          }
        ]
      }""")

  "OrderNoticesConsumed" in:
    testJson[OrderEvent](OrderNoticesConsumed(failed = true),
      json"""
      {
        "TYPE": "OrderNoticesConsumed",
        "failed": true
      }""")

    testJsonDecoder[OrderEvent](OrderNoticesConsumed(),
      json"""
      {
        "TYPE": "OrderNoticesConsumed"
      }""")

  "OrderNoticesConsumptionStarted" in:
    testJson[OrderEvent](OrderNoticesConsumptionStarted(Vector(
      BoardPath("BOARD") \ "NOTICE")),
      json"""
      {
        "TYPE": "OrderNoticesConsumptionStarted",
        "noticeIds": [
          [ "BOARD", "NOTICE" ]
        ]
      }""")

    // COMPATIBLE with v2.7.3
    testJsonDecoder[OrderEvent](OrderNoticesConsumptionStarted(Vector(
      BoardPath("BOARD") \ "NOTICE")),
      json"""
      {
        "TYPE": "OrderNoticesConsumptionStarted",
        "consumptions": [
          {
            "boardPath": "BOARD",
            "noticeId": "NOTICE"
          }
        ]
      }""")

  // COMPATIBLE with v2.3
  "OrderNoticeRead" in:
    testJsonDecoder[OrderEvent](OrderNoticesRead,
      json"""
      {
        "TYPE": "OrderNoticeRead"
      }""")

  "OrderNoticesRead" in:
    testJson[OrderEvent](OrderNoticesRead,
      json"""
      {
        "TYPE": "OrderNoticesRead"
      }""")

  "OrderStickySubagentEntered" in:
    testJson[OrderEvent](
      OrderStickySubagentEntered(
        AgentPath("AGENT"),
        Some(SubagentBundleId("SUBAGENT-BUNDLE"))),
      json"""
      {
        "TYPE": "OrderStickySubagentEntered",
        "agentPath": "AGENT",
        "subagentBundleId": "SUBAGENT-BUNDLE"
      }""")

  "OrderStickySubagentEntered until v2.7.1" in:
    testJsonDecoder[OrderEvent](
      OrderStickySubagentEntered(
        AgentPath("AGENT"),
        Some(SubagentBundleId("SUBAGENT-BUNDLE"))),
      json"""
      {
        "TYPE": "OrderStickySubagentEntered",
        "agentPath": "AGENT",
        "subagentSelectionId": "SUBAGENT-BUNDLE"
      }""")

  "OrderStickySubagentLeaved" in:
    testJson[OrderEvent](OrderStickySubagentLeaved,
      json"""
      {
        "TYPE": "OrderStickySubagentLeaved"
      }""")

  "OrderPrompted" in:
    testJson[OrderEvent](OrderPrompted(StringValue("QUESTION")), json"""
      {
        "TYPE": "OrderPrompted",
        "question": "QUESTION"
      }""")

  "OrderPromptAnswered" in:
    testJson[OrderEvent](OrderPromptAnswered(), json"""
      {
        "TYPE": "OrderPromptAnswered"
      }""")

  "OrderSleeping" in:
    testJson[OrderEvent](OrderSleeping(ts"2024-12-18T12:00:00Z"),
      json"""
      {
        "TYPE": "OrderSleeping",
        "until": 1734523200000
      }""")

  "OrderTransferred" in:
    testJson[OrderEvent](OrderTransferred(WorkflowPath("WORKFLOW") ~ "v2" /: Position(7)),
      json"""
      {
        "TYPE": "OrderTransferred",
        "workflowPosition": {
          "workflowId": {
            "path": "WORKFLOW",
            "versionId": "v2"
          },
          "position": [7]
        }
      }""")

  "OrderBroken" in:
    testJson[OrderEvent](OrderBroken(), json"""
      {
        "TYPE": "OrderBroken"
      }""")

    testJson[OrderEvent](
      OrderBroken(Problem("PROBLEM")): @nowarn("msg=deprecated"),
      json"""
      {
        "TYPE": "OrderBroken",
        "problem": {
          "message": "PROBLEM"
        }
      }""")

  if sys.props contains "test.speed" then "Speed" in:
    val n = 10000
    val event: Stamped[KeyedEvent[OrderEvent]] =
      Stamped(12345678L, Timestamp.ofEpochMilli(1),
        OrderId("ORDER") <-:
          OrderAdded(
            WorkflowPath("WORKFLOW") ~ "VERSION",
            arguments = Map("KEY" -> StringValue("VALUE"))))
    val jsonString = event.asJson.compactPrint
    println(f"${"Serialize"}%-20s Deserialize")
    for _ <- 1 to 10 do
      val circeSerialize =  measure(event.asJson.compactPrint)
      val circeDeserialize = measure(jsonString.parseJsonOrThrow.as[OrderEvent].orThrow: OrderEvent)
      println(f"$circeSerialize%-20s $circeDeserialize%-20s")
    def measure[A](serialize: => Unit) =
      val t = System.nanoTime
      for _ <- 1 to n do serialize
      val d = (System.nanoTime - t).nanoseconds
      s"${d.pretty} ${n*1000/d.toMillis}/s"
