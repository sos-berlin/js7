package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCatched, OrderDetachable, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTerminated, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.BranchId.{Then, try_}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.TryTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

final class TryTest extends FreeSpec
{
  "Nested try catch with outer non-failing catch, OrderFinished" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, FinishingWorkflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(ExecutablePath(s"/OKAY$sh"), ":")
        a.writeExecutable(ExecutablePath(s"/FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
        a.writeExecutable(ExecutablePath(s"/FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
      }
      directoryProvider.run { (master, _) =>
        val orderId = OrderId("🔺")
        master.addOrderBlocking(FreshOrder(orderId, FinishingWorkflow.id.path))
        master.eventWatch.await[OrderFinished](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], ExpectedFinishedEvents)
      }
    }
  }

  "Nested try catch with failing catch, OrderFailed" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, StoppingWorkflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(ExecutablePath(s"/FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
        a.writeExecutable(ExecutablePath(s"/FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
      }
      directoryProvider.run { (master, _) =>
        val orderId = OrderId("❌")
        master.addOrderBlocking(FreshOrder(orderId, StoppingWorkflow.id.path))
        master.eventWatch.await[OrderFailed](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], ExpectedStoppedEvent)
      }
    }
  }

  "try - if - fail" in {
    val workflow = WorkflowParser.parse(WorkflowPath("/TRY-IF") ~ "INITIAL",
      s"""define workflow {
         |  try {
         |    execute executable="/OKAY$sh", agent="AGENT";
         |    if (true) {
         |      fail;
         |    }
         |  } catch {
         |    execute executable="/OKAY$sh", agent="AGENT";
         |  }
         |  execute executable="/OKAY$sh", agent="AGENT";
         |}""".stripMargin).orThrow
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(ExecutablePath(s"/OKAY$sh"), ":")
        a.writeExecutable(ExecutablePath(s"/FAIL$sh"), if (isWindows) "@exit 1" else "exit 1")
      }
      directoryProvider.run { (master, _) =>
        val orderId = OrderId("⭕")
        master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        master.eventWatch.await[OrderFinished](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], Vector(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / try_(0) % 0),
          OrderAttachable(TestAgentRefPath),
          OrderTransferredToAgent(TestAgentRefPath),
          OrderStarted,
          OrderProcessingStarted,
          OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
          OrderMoved(Position(0) / try_(0) % 1 / Then % 0),
          OrderCatched(Outcome.Failed(ReturnCode(0)), Position(0) / "catch+0" % 0),
          OrderProcessingStarted,
          OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
          OrderMoved(Position(1)),
          OrderProcessingStarted,
          OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
          OrderMoved(Position(2)),
          OrderDetachable,
          OrderTransferredToMaster,
          OrderFinished))
      }
    }
  }

  "fork - fail" in {
    val workflow = WorkflowParser.parse(WorkflowPath("/TRY-IF") ~ "INITIAL",
      s"""define workflow {
         |  try {
         |    fork {
         |      "🥕": { execute executable="/OKAY$sh", agent="AGENT"; },
         |      "🍋": { execute executable="/FAIL-1$sh", agent="AGENT"; },
         |      "🌶": { if (true) execute executable="/FAIL-2$sh", agent="AGENT"; }
         |    }
         |    execute executable="/NEVER$sh", agent="AGENT";
         |  } catch {
         |    execute executable="/OKAY$sh", agent="AGENT";
         |  }
         |}""".stripMargin).orThrow
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(ExecutablePath(s"/OKAY$sh"), ":")
        a.writeExecutable(ExecutablePath(s"/FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
        a.writeExecutable(ExecutablePath(s"/FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
        a.writeExecutable(ExecutablePath(s"/NEVER$sh"), if (isWindows) "@exit 3" else "exit 3")
      }
      directoryProvider.run { (master, _) =>
        val orderId = OrderId("🔴")
        master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        master.eventWatch.await[OrderTerminated](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], Vector(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderStarted,
          OrderForked(Vector(
            OrderForked.Child("🥕", OrderId("🔴/🥕")),
            OrderForked.Child("🍋", OrderId("🔴/🍋")),
            OrderForked.Child("🌶", OrderId("🔴/🌶")))),
          OrderJoined(Outcome.Failed(ReturnCode(0))),
          OrderCatched(Outcome.Failed(ReturnCode(0)), Position(0) / "catch+0" % 0),
          OrderAttachable(TestAgentRefPath),
          OrderTransferredToAgent(TestAgentRefPath),
          OrderProcessingStarted,
          OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
          OrderMoved(Position(1)),
          OrderDetachable,
          OrderTransferredToMaster,
          OrderFinished))
        checkEventSeq(OrderId("🔴/🍋"), master.eventWatch.all[OrderEvent], Vector(
          OrderAttachable(TestAgentRefPath),
          OrderTransferredToAgent(TestAgentRefPath),
          OrderProcessingStarted,
          OrderProcessed(Outcome.Failed(None,ReturnCode(1))),
          OrderFailedInFork(Outcome.Failed(None,ReturnCode(1), Map.empty)),
          OrderDetachable,
          OrderTransferredToMaster))
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]], expected: Vector[OrderEvent]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == expected)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object TryTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val finishingScript = s"""
     |define workflow {
     |  try {                                                 // #0
     |    try {                                               // #0/0#0
     |      execute executable="/FAIL-1$sh", agent="AGENT";   // #0/0#0/0#0   OrderCatched
     |      execute executable="/OKAY$sh", agent="AGENT";     // #0/0#0/0#1   skipped
     |    } catch {
     |      execute executable="/FAIL-2$sh", agent="AGENT";   // #0/0#0/1#0   OrderCatched
     |    }
     |    execute executable="/OKAY$sh", agent="AGENT";       // #0/0#1
     |  } catch {}
     |  execute executable="/OKAY$sh", agent="AGENT";         // #1
     |}""".stripMargin
  private val FinishingWorkflow = WorkflowParser.parse(WorkflowPath("/FINISHING") ~ "INITIAL", finishingScript).orThrow

  private val ExpectedFinishedEvents = Vector(
    OrderAdded(FinishingWorkflow.id),
    OrderMoved(Position(0) / "try+0" % 0 / "try+0" % 0),
    OrderAttachable(TestAgentRefPath),
    OrderTransferredToAgent(TestAgentRefPath),

    OrderStarted,
    OrderProcessingStarted,
    OrderProcessed(Outcome.Failed(ReturnCode(1))),
    OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / "try+0" % 0 / "catch+0" % 0),

    OrderProcessingStarted,
    OrderProcessed(Outcome.Failed(ReturnCode(2))),
    OrderCatched(Outcome.Failed(ReturnCode(2)), Position(1)),  // Empty catch-block, so Order is moved to outer block

    OrderProcessingStarted,
    OrderProcessed(Outcome.succeeded),
    OrderMoved(Position(2)),

    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)

  private val stoppingScript = s"""
     |define workflow {
     |  try {                                               // #0
     |    execute executable="/FAIL-1$sh", agent="AGENT";   // #0/0#0  OrderCatched
     |  } catch {
     |    execute executable="/FAIL-2$sh", agent="AGENT";   // #0/1#0  OrderFailed
     |  }
     |}""".stripMargin
  private val StoppingWorkflow = WorkflowParser.parse(WorkflowPath("/STOPPING") ~ "INITIAL", stoppingScript).orThrow

  private val ExpectedStoppedEvent = Vector(
    OrderAdded(StoppingWorkflow.id),
    OrderMoved(Position(0) / "try+0" % 0),
    OrderAttachable(TestAgentRefPath),
    OrderTransferredToAgent(TestAgentRefPath),

    OrderStarted,
    OrderProcessingStarted,
    OrderProcessed(Outcome.Failed(ReturnCode(1))),
    OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / "catch+0" % 0),

    OrderProcessingStarted,
    OrderProcessed(Outcome.Failed(ReturnCode(2))),
    OrderFailed(Outcome.Failed(ReturnCode(2))))
}
