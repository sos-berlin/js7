package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCatched, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, If, TryInstruction}
import js7.data.workflow.position.BranchId.{Then, try_}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.TryTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{ControllerAgentForScalaTest, TestItemUpdater}
import monix.execution.Scheduler.Implicits.traced
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

final class TryTest
extends AnyFreeSpec
with ControllerAgentForScalaTest
with TestItemUpdater
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.delay = 1ms"""

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "(prepare)" in {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(RelativePathExecutable(s"OKAY$sh"), ":")
      a.writeExecutable(RelativePathExecutable(s"FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
      a.writeExecutable(RelativePathExecutable(s"FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
      a.writeExecutable(RelativePathExecutable(s"NEVER$sh"), if (isWindows) "@exit 3" else "exit 3")
    }
  }

  "Nested try catch with outer non-failing catch, OrderFinished" in {
    val finishingScript =
      s"""
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
    val workflow = WorkflowParser.parse(WorkflowPath("FINISHING"), finishingScript).orThrow
    val Some(v) = updateItems(workflow)

    val orderId = OrderId("🔺")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderFinished](_.key == orderId)
    checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderAdded(workflow.path ~ v),
      OrderMoved(Position(0) / "try+0" % 0 / "try+0" % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),

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
      OrderFinished))
    assert(controllerState.idToOrder(orderId).lastOutcome == Outcome.succeededRC0)
  }

  "Nested try catch with failing catch, OrderFailed" in {
    val workflowPath = WorkflowPath("STOPPING")
    val Some(v) = updateItems(Workflow(
      workflowPath,
      Seq(
        TryInstruction(
          tryWorkflow = Workflow.of(Fail()),
          catchWorkflow = Workflow.of(Fail())))))

    val orderId = OrderId("❌")
    controller.addOrderBlocking(FreshOrder(orderId, workflowPath))
    controller.eventWatch.await[OrderFailed](_.key == orderId)
    checkEventSeq(
      orderId,
      controller.eventWatch.allKeyedEvents[OrderEvent],
      Vector(
        OrderAdded(workflowPath ~ v),
        OrderMoved(Position(0) / "try+0" % 0),
        OrderStarted,
        OrderCatched(Position(0) / "catch+0" % 0, Some(Outcome.failed)),
        OrderFailed(Position(0) / "catch+0" % 0, Some(Outcome.failed))))
    assert(controllerState.idToOrder(orderId).lastOutcome == Outcome.failed)
  }

  "try - if - fail" in {
    val workflow = Workflow(
      WorkflowPath("TRY-IF"),
      Seq(
        TryInstruction(
          tryWorkflow = Workflow.of(
            EmptyJob.execute(agentPath),
            If(expr("true"),
              Workflow.of(Fail()))),
          catchWorkflow = Workflow.of(
            EmptyJob.execute(agentPath))),
        EmptyJob.execute(agentPath)))
    val Some(v) = updateItems(workflow)

    val orderId = OrderId("⭕")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderFinished](_.key == orderId)

    checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderAdded(workflow.path ~ v),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / try_(0) % 1 / Then % 0),
      OrderCatched(Position(0) / "catch+0" % 0, Some(Outcome.failed)),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
    assert(controllerState.idToOrder(orderId).lastOutcome == Outcome.succeeded)
  }

  "fork - fail" in {
    val workflow = WorkflowParser.parse(WorkflowPath("FORK-FAIL"),
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
    val Some(v) = updateItems(workflow)

    val orderId = OrderId("🔴")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderAdded(workflow.path ~ v),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderForked(Vector(
        OrderForked.Child("🥕", OrderId("🔴|🥕")),
        OrderForked.Child("🍋", OrderId("🔴|🍋")),
        OrderForked.Child("🌶", OrderId("🔴|🌶")))),
      OrderDetachable,
      OrderDetached,
      OrderJoined(Outcome.Failed(Some("Order:🔴|🍋 failed;\nOrder:🔴|🌶 failed"))),
      OrderCatched(Position(0) / "catch+0" % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
    checkEventSeq(OrderId("🔴|🍋"), controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(None, NamedValues.rc(1))),
      OrderDetachable,
      OrderDetached,
      OrderFailedInFork(Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0)))
    assert(controllerState.idToOrder(orderId).lastOutcome == Outcome.succeededRC0)
  }

  private def checkEventSeq(
    orderId: OrderId,
    keyedEvents: IterableOnce[KeyedEvent[OrderEvent]],
    expected: Vector[OrderEvent])
    (implicit pos: source.Position)
  : Unit = {
      val events = keyedEvents.iterator.filter(_.key == orderId).map(_.event).toVector
      assert(events == expected)
    }
}

object TryTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
}
