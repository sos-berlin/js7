package js7.tests

import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCatched, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.position.BranchId.{Then, try_}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.TryTest._
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class TryTest extends AnyFreeSpec
{
  "Nested try catch with outer non-failing catch, OrderFinished" in {
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, FinishingWorkflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(RelativePathExecutable(s"OKAY$sh"), ":")
        a.writeExecutable(RelativePathExecutable(s"FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
        a.writeExecutable(RelativePathExecutable(s"FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
      }
      directoryProvider.run { (controller, _) =>
        val orderId = OrderId("🔺")
        controller.addOrderBlocking(FreshOrder(orderId, FinishingWorkflow.id.path))
        controller.eventWatch.await[OrderFinished](_.key == orderId)
        checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], ExpectedFinishedEvents)
      }
    }
  }

  "Nested try catch with failing catch, OrderFailed" in {
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, StoppingWorkflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(RelativePathExecutable(s"FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
        a.writeExecutable(RelativePathExecutable(s"FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
      }
      directoryProvider.run { (controller, _) =>
        val orderId = OrderId("❌")
        controller.addOrderBlocking(FreshOrder(orderId, StoppingWorkflow.id.path))
        controller.eventWatch.await[OrderFailed](_.key == orderId)
        checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], ExpectedStoppedEvent)
      }
    }
  }

  "try - if - fail" in {
    val workflow = WorkflowParser.parse(WorkflowPath("TRY-IF") ~ "INITIAL",
      s"""define workflow {
         |  try {
         |    execute executable="OKAY$sh", agent="AGENT";
         |    if (true) {
         |      fail;
         |    }
         |  } catch {
         |    execute executable="OKAY$sh", agent="AGENT";
         |  }
         |  execute executable="OKAY$sh", agent="AGENT";
         |}""".stripMargin).orThrow
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, workflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(RelativePathExecutable(s"OKAY$sh"), ":")
        a.writeExecutable(RelativePathExecutable(s"FAIL$sh"), if (isWindows) "@exit 1" else "exit 1")
      }
      directoryProvider.run { (controller, _) =>
        val orderId = OrderId("⭕")
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        controller.eventWatch.await[OrderFinished](_.key == orderId)
        checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / try_(0) % 0),
          OrderAttachable(TestAgentPath),
          OrderAttached(TestAgentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
          OrderMoved(Position(0) / try_(0) % 1 / Then % 0),
          OrderCatched(Position(0) / "catch+0" % 0, Some(Outcome.failed)),
          OrderProcessingStarted(subagentId),
          OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
          OrderMoved(Position(1)),
          OrderProcessingStarted(subagentId),
          OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
          OrderMoved(Position(2)),
          OrderDetachable,
          OrderDetached,
          OrderFinished))
      }
    }
  }

  "fork - fail" in {
    val workflow = WorkflowParser.parse(WorkflowPath("TRY-IF") ~ "INITIAL",
      s"""define workflow {
         |  try {
         |    fork (joinIfFailed=true) {
         |      "🥕": { execute executable="OKAY$sh", agent="AGENT"; },
         |      "🍋": { execute executable="FAIL-1$sh", agent="AGENT"; },
         |      "🌶": { if (true) execute executable="FAIL-2$sh", agent="AGENT"; }
         |    }
         |    execute executable="NEVER$sh", agent="AGENT";
         |  } catch {
         |    execute executable="OKAY$sh", agent="AGENT";
         |  }
         |}""".stripMargin).orThrow
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, workflow :: Nil, testName = Some("TryTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.writeExecutable(RelativePathExecutable(s"OKAY$sh"), ":")
        a.writeExecutable(RelativePathExecutable(s"FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
        a.writeExecutable(RelativePathExecutable(s"FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
        a.writeExecutable(RelativePathExecutable(s"NEVER$sh"), if (isWindows) "@exit 3" else "exit 3")
      }
      directoryProvider.run { (controller, _) =>
        val orderId = OrderId("🔴")
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        controller.eventWatch.await[OrderTerminated](_.key == orderId)
        checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderStarted,
          OrderAttachable(TestAgentPath),
          OrderAttached(TestAgentPath),
          OrderForked(Vector(
            OrderForked.Child("🥕", OrderId("🔴|🥕")),
            OrderForked.Child("🍋", OrderId("🔴|🍋")),
            OrderForked.Child("🌶", OrderId("🔴|🌶")))),
          OrderDetachable,
          OrderDetached,
          OrderJoined(Outcome.Failed(Some("Order:🔴|🍋 failed;\nOrder:🔴|🌶 failed"))),
          OrderCatched(Position(0) / "catch+0" % 0),
          OrderAttachable(TestAgentPath),
          OrderAttached(TestAgentPath),
          OrderProcessingStarted(subagentId),
          OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
          OrderMoved(Position(1)),
          OrderDetachable,
          OrderDetached,
          OrderFinished))
        checkEventSeq(OrderId("🔴|🍋"), controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
          OrderProcessingStarted(subagentId),
          OrderProcessed(Outcome.Failed(None, NamedValues.rc(1))),
          OrderFailedInFork(Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0),
          OrderDetachable,
          OrderDetached))
      }
    }
  }

  private def checkEventSeq(
    orderId: OrderId,
    keyedEvents: IterableOnce[KeyedEvent[OrderEvent]],
    expected: Vector[OrderEvent])
  : Unit = {
      val events = keyedEvents.iterator.filter(_.key == orderId).map(_.event).toVector
      assert(events == expected)
    }
}

object TryTest
{
  private val TestAgentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(TestAgentPath)
  private val finishingScript = s"""
     |define workflow {
     |  try {                                                // :0
     |    try {                                              // :0/try:0
     |      execute executable="FAIL-1$sh", agent="AGENT";   // :0/try:0/try:0   OrderCatched
     |      execute executable="OKAY$sh", agent="AGENT";     // :0/try:0/try:1   skipped
     |    } catch {
     |      execute executable="FAIL-2$sh", agent="AGENT";   // :0/try:0/catch:0   OrderCatched
     |    }
     |    execute executable="OKAY$sh", agent="AGENT";       // :0/try:1
     |  } catch {}
     |  execute executable="OKAY$sh", agent="AGENT";         // :1
     |}""".stripMargin
  private val FinishingWorkflow = WorkflowParser.parse(WorkflowPath("FINISHING") ~ "INITIAL", finishingScript).orThrow

  private val ExpectedFinishedEvents = Vector(
    OrderAdded(FinishingWorkflow.id),
    OrderMoved(Position(0) / "try+0" % 0 / "try+0" % 0),
    OrderAttachable(TestAgentPath),
    OrderAttached(TestAgentPath),

    OrderStarted,
    OrderProcessingStarted(subagentId),
    OrderProcessed(Outcome.Failed.rc(1)),
    OrderCatched(Position(0) / "try+0" % 0 / "catch+0" % 0),

    OrderProcessingStarted(subagentId),
    OrderProcessed(Outcome.Failed.rc(2)),
    OrderCatched(Position(0) / "catch+0" % 0),
    OrderMoved(Position(1)),

    OrderProcessingStarted(subagentId),
    OrderProcessed(Outcome.Succeeded.rc(0)),
    OrderMoved(Position(2)),

    OrderDetachable,
    OrderDetached,
    OrderFinished)

  private val stoppingScript = s"""
     |define workflow {
     |  try {                                              // :0
     |    execute executable="FAIL-1$sh", agent="AGENT";   // :0/try:0  OrderCatched
     |  } catch {
     |    execute executable="FAIL-2$sh", agent="AGENT";   // :0/catch:0  OrderFailed
     |  }
     |}""".stripMargin
  private val StoppingWorkflow = WorkflowParser.parse(WorkflowPath("STOPPING") ~ "INITIAL", stoppingScript).orThrow

  private val ExpectedStoppedEvent = Vector(
    OrderAdded(StoppingWorkflow.id),
    OrderMoved(Position(0) / "try+0" % 0),
    OrderAttachable(TestAgentPath),
    OrderAttached(TestAgentPath),

    OrderStarted,
    OrderProcessingStarted(subagentId),
    OrderProcessed(Outcome.Failed.rc(1)),
    OrderCatched(Position(0) / "catch+0" % 0),

    OrderProcessingStarted(subagentId),
    OrderProcessed(Outcome.Failed.rc(2)),
    OrderDetachable,
    OrderDetached,
    OrderFailed(Position(0) / "catch+0" % 0))
}
