package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCaught, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.NamedValues
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, If, TryInstruction}
import js7.data.workflow.position.BranchId.{Then, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import js7.tests.TryTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import org.scalactic.source

final class TryTest
extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

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

  "(prepare)" in:
    for a <- directoryProvider.agentEnvs do
      a.writeExecutable(RelativePathExecutable(s"OKAY$sh"), ":")
      a.writeExecutable(RelativePathExecutable(s"FAIL-1$sh"), if isWindows then "@exit 1" else "exit 1")
      a.writeExecutable(RelativePathExecutable(s"FAIL-2$sh"), if isWindows then "@exit 2" else "exit 2")
      a.writeExecutable(RelativePathExecutable(s"NEVER$sh"), if isWindows then "@exit 3" else "exit 3")

  "Nested try catch with outer non-failing catch, OrderFinished" in:
    val finishingScript =
      s"""
          |define workflow {
          |  try {                                                // :0
          |    try {                                              // :0/try:0
          |      execute executable="FAIL-1$sh", agent="AGENT";   // :0/try:0/try:0   OrderCaught
          |      execute executable="OKAY$sh", agent="AGENT";     // :0/try:0/try:1   skipped
          |    } catch {
          |      execute executable="FAIL-2$sh", agent="AGENT";   // :0/try:0/catch:0   OrderCaught
          |    }
          |    execute executable="OKAY$sh", agent="AGENT";       // :0/try:1
          |  } catch {}
          |  execute executable="OKAY$sh", agent="AGENT";         // :1
          |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("FINISHING"), finishingScript).orThrow
    val Some(v) = updateItems(workflow): @unchecked

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
      OrderProcessed(OrderOutcome.Failed.rc(1)),
      OrderCaught(Position(0) / "try+0" % 0 / "catch+0" % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed.rc(2)),
      OrderCaught(Position(0) / "catch+0" % 0),
      OrderMoved(Position(1)),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Succeeded.rc(0)),
      OrderMoved(Position(2)),

      OrderDetachable,
      OrderDetached,
      OrderFinished()))
    assert(controllerState.idToOrder(orderId).lastOutcome == OrderOutcome.succeededRC0)

  "Nested try catch with failing catch, OrderFailed" in:
    val workflowPath = WorkflowPath("STOPPING")
    val Some(v) = updateItems(Workflow(
      workflowPath,
      Seq(
        TryInstruction(
          tryWorkflow = Workflow.of(Fail()),
          catchWorkflow = Workflow.of(Fail()))))): @unchecked

    val orderId = OrderId("🔻")
    controller.addOrderBlocking(FreshOrder(orderId, workflowPath))
    controller.eventWatch.await[OrderFailed](_.key == orderId)
    checkEventSeq(
      orderId,
      controller.eventWatch.allKeyedEvents[OrderEvent],
      Vector(
        OrderAdded(workflowPath ~ v),
        OrderMoved(Position(0) / "try+0" % 0),
        OrderStarted,
        OrderOutcomeAdded(OrderOutcome.failed),
        OrderCaught(Position(0) / "catch+0" % 0),
        OrderOutcomeAdded(OrderOutcome.failed),
        OrderFailed(Position(0) / "catch+0" % 0)))
    assert(controllerState.idToOrder(orderId).lastOutcome == OrderOutcome.failed)

  "Empty catch" in:
    val workflowPath = WorkflowPath("TRY-IF")
    val Some(v) = updateItems(Workflow(
      workflowPath,
      Seq(
        TryInstruction(
          tryWorkflow = Workflow.of(Fail()),
          catchWorkflow = Workflow.empty)))): @unchecked

    val orderId = OrderId("♣️")
    controller.addOrderBlocking(FreshOrder(orderId, workflowPath))
    controller.eventWatch.await[OrderFinished](_.key == orderId)

    checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderAdded(workflowPath ~ v),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderStarted,
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / "catch+0" % 0),
      OrderMoved(Position(1)),
      OrderFinished()))
    assert(controllerState.idToOrder(orderId).lastOutcome == OrderOutcome.succeeded)

  "try - if - fail" in:
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
    val Some(v) = updateItems(workflow): @unchecked

    val orderId = OrderId("♦️")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderFinished](_.key == orderId)

    checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderAdded(workflow.path ~ v),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / try_(0) % 1 / Then % 0),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / "catch+0" % 0),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1)),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))
    assert(controllerState.idToOrder(orderId).lastOutcome == OrderOutcome.succeeded)

  "fork - fail" in:
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
    val Some(v) = updateItems(workflow): @unchecked

    val orderId = OrderId("♠️")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderTerminated](_.key == orderId)

    checkEventSeq(orderId, controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderAdded(workflow.path ~ v),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderForked(Vector(
        "🥕" -> OrderId("♠️|🥕"),
        "🍋" -> OrderId("♠️|🍋"),
        "🌶" -> OrderId("♠️|🌶"))),
      OrderDetachable,
      OrderDetached,
      OrderJoined(OrderOutcome.Failed(Some("Order:♠️|🍋 Failed;\nOrder:♠️|🌶 Failed"))),
      OrderCaught(Position(0) / "catch+0" % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))

    checkEventSeq(OrderId("♠️|🍋"), controller.eventWatch.allKeyedEvents[OrderEvent], Vector(
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed(None, NamedValues.rc(1))),
      OrderDetachable,
      OrderDetached,
      OrderFailedInFork(Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0)))

    assert(controllerState.idToOrder(orderId).lastOutcome == OrderOutcome.succeededRC0)

  private def checkEventSeq(
    orderId: OrderId,
    keyedEvents: IterableOnce[KeyedEvent[OrderEvent]],
    expected: Vector[OrderEvent])
    (implicit pos: source.Position)
  : Unit =
      val events = keyedEvents.iterator.filter(_.key == orderId).map(_.event).toVector
      assert(events == expected)


object TryTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
