package js7.tests

import cats.syntax.option.*
import izumi.reflect.Tag
import js7.base.configutils.Configs.*
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.Problems.GoOrderInapplicableProblem
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, GoOrder, ResumeOrders, SuspendOrders}
import js7.data.event.{EventId, EventRequest, EventSeq}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAwoke, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderGoMarked, OrderGoes, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderResumed, OrderResumptionMarked, OrderRetrying, OrderStarted, OrderStateReset, OrderSuspended, OrderSuspensionMarked, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.expression.Expression
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.instructions.{Fail, If, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import js7.tests.RetryTest.*
import js7.tests.jobs.{EmptyJob, FailingJob}
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import org.scalatest.Assertion
import scala.concurrent.duration.*
import scala.reflect.ClassTag
import scala.util.Random

final class RetryTest
extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  protected val agentPaths = agentPath :: Nil
  protected val items = Nil

  override def beforeAll() =
    for a <- directoryProvider.agentEnvs do
      a.writeExecutable(RelativePathExecutable(s"OKAY$sh"), ":")
      a.writeExecutable(RelativePathExecutable(s"FAIL-1$sh"), if isWindows then "@exit 1" else "exit 1")
      a.writeExecutable(RelativePathExecutable(s"FAIL-2$sh"), if isWindows then "@exit 2" else "exit 2")
    super.beforeAll()

  "Nested try catch (using tryCount)" in:
    val workflow = Workflow.of(WorkflowPath("TEST"),
      TryInstruction(
        Workflow.of:
          Fail(),
        Workflow.of:
          If(expr("tryCount < 2")):
            TryInstruction(
              Workflow.of:
                Retry(),
              Workflow.empty)))

    withItem(workflow): workflow =>
      val expectedEvents = Vector(
        OrderAdded(workflow.id),
        OrderMoved(Position(0) / try_(0) % 0),
        OrderStarted,
        OrderOutcomeAdded(OrderOutcome.failed),

        OrderCaught(Position(0) / catch_(0) % 0),
        OrderMoved(Position(0) / catch_(0) % 0 / Then % 0 / try_(0) % 0),
        OrderRetrying(),
        OrderMoved(Position(0) / try_(1) % 0),

        OrderOutcomeAdded(OrderOutcome.failed),
        OrderCaught(Position(0) / catch_(1) % 0),   // Retry limit reached
        OrderMoved(Position(1)),
        OrderFinished())

      val orderId = OrderId("🔺")
      val afterEventId = eventWatch.lastAddedEventId
      controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
      awaitAndCheckEventSeq[OrderTerminated](afterEventId, orderId, expectedEvents)

  "Nested try catch with outer non-failing catch" in:
    val workflowNotation = s"""
       |define workflow {
       |  try {                                                  // :0
       |    try {                                                // :0/try:0
       |      execute executable="OKAY$sh", agent="AGENT";       // :0/try:0/try:0
       |      try {                                              // :0/try:0/try:1
       |        execute executable="FAIL-1$sh", agent="AGENT";   // :0/try:0/try:1/try:0   OrderCaught
       |        execute executable="OKAY$sh", agent="AGENT";     // :0/try:0/try:1/try:1   skipped
       |      } catch if (tryCount < 3) retry else fail;         // :0/try:0/try:1/catch:0
       |      execute executable="OKAY$sh", agent="AGENT";       // :0/try:0/try:2
       |    } catch if (tryCount < 2) retry else fail;
       |  } catch execute executable="OKAY$sh", agent="AGENT";   // :0/catch:0
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("TEST"), workflowNotation).orThrow
    val versionId = updateVersionedItems(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(0) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(0) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(0) % 0 / Then % 0),

      OrderRetrying(),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(1) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(1) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(1) % 0 / Then % 0),

      OrderRetrying(),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(2) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(2) % 0),   // Retry limit reached
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(2) % 0 / Else % 0),

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / try_(0) % 0 / catch_(0) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / catch_(0) % 0 / Then % 0),

      OrderRetrying(),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(0) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(0) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(0) % 0 / Then % 0),
      OrderRetrying(),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(1) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(1) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(1) % 0 / Then % 0),
      OrderRetrying(),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(2) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(2) % 0),  // Retry limit reached
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(2) % 0 / Else % 0),

      OrderOutcomeAdded(OrderOutcome.failed),  // Retry limit reached
      OrderCaught(Position(0) / try_(0) % 0 / catch_(1) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / catch_(1) % 0 / Else % 0),

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / catch_(0) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),

      OrderDetachable,
      OrderDetached,
      OrderFinished())

    val orderId = OrderId("🔷")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFinished](afterEventId, orderId, expectedEvents)

  // For test of retryDelays see RetryDelayTest

  "maxTries=3, special handling of 'catch retry'" in:
    val workflowNotation = s"""
       |define workflow {
       |  try (maxTries=3) fail;
       |  catch retry;
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("TEST"), workflowNotation).orThrow
    val versionId = updateVersionedItems(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderStarted,

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / catch_(0) % 0),
      OrderRetrying(),
      OrderMoved(Position(0) / try_(1) % 0),

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / catch_(1) % 0),
      OrderRetrying(),
      OrderMoved(Position(0) / try_(2) % 0),

      // No OrderCaught here! OrderFailed has OrderOutcome of last failed instruction in try block
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderFailed(Position(0) / try_(2) % 0))

    val orderId = OrderId("🔶")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFailed](afterEventId, orderId, expectedEvents)

  "maxTries=3, standard handling, stopping at retry instruction" in:
    val workflowNotation = s"""
       |define workflow {
       |  try (maxTries=3) fail;
       |  catch if (true) retry;
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("TEST"), workflowNotation).orThrow
    val versionId = updateVersionedItems(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderStarted,

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / catch_(0) % 0),
      OrderMoved(Position(0) / catch_(0) % 0 / Then % 0),
      OrderRetrying(),
      OrderMoved(Position(0) / try_(1) % 0),

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / catch_(1) % 0),
      OrderMoved(Position(0) / catch_(1) % 0 / Then % 0),
      OrderRetrying(),
      OrderMoved(Position(0) / try_(2) % 0),

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(0) / catch_(2) % 0),
      OrderMoved(Position(0) / catch_(2) % 0 / Then % 0),
      OrderFailed(Position(0) / catch_(2) % 0 / Then % 0))

    val orderId = OrderId("♣️")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFailed](afterEventId, orderId, expectedEvents)

  "JS-2094 Omitting maxTries means unlimited retries" - {
    "Retry at first position" in:
      val workflow = Workflow(WorkflowPath("OMITTED-MAX-TRIES-FIRST"), Seq(
        TryInstruction(
          Workflow.of(
            Fail()),
          Workflow.of(
            Retry()),
          retryDelays = Some(Vector(10.ms)))))

      withItem(workflow) { workflow =>
        val orderId = OrderId("🟦")
        var eventId = eventWatch.lastAddedEventId
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        for _ <- 1 to 10 do
          eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId
        controller.api
          .executeCommand(CancelOrders(Seq(orderId), CancellationMode.FreshOrStarted()))
          .await(99.s).orThrow
        assert(eventWatch
          .keyedEvents[OrderEvent](_.key == orderId, after = EventId.BeforeFirst)
          .take(24)
          .map(_.event)
          .map {
            case e: OrderRetrying => e.copy(delayedUntil = None)
            case e => e
          } == Seq(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / try_(0) % 0),
          OrderStarted,

          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / catch_(0) % 0),
          OrderRetrying(),

          OrderAwoke,
          OrderMoved(Position(0) / try_(1) % 0),
          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / catch_(1) % 0),
          OrderRetrying(),

          OrderAwoke,
          OrderMoved(Position(0) / try_(2) % 0),
          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / catch_(2) % 0),
          OrderRetrying(),

          OrderAwoke,
          OrderMoved(Position(0) / try_(3) % 0),
          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / catch_(3) % 0),
          OrderRetrying(),

          OrderAwoke,
          OrderMoved(Position(0) / try_(4) % 0),
          OrderOutcomeAdded(OrderOutcome.failed)))
      }

    "Retry not at first position" in:
      val workflow = Workflow(WorkflowPath("OMITTED-MAX-TRIES-SECOND"), Seq(
        TryInstruction(
          Workflow.of(
            Fail()),
          Workflow.of(
            EmptyJob.execute(agentPath),
            Retry()),
          retryDelays = Some(Vector(10.ms)))))

      withItem(workflow) { workflow =>
        val orderId = OrderId("🟪")
        var eventId = eventWatch.lastAddedEventId
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        for _ <- 1 to 3 do
          eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId
        controller.api
          .executeCommand(CancelOrders(Seq(orderId), CancellationMode.FreshOrStarted()))
          .await(99.s).orThrow
        eventWatch.await[OrderCancelled](_.key == orderId)

        assert(eventWatch
          .keyedEvents[OrderEvent](_.key == orderId, after = EventId.BeforeFirst)
          .take(27)
          .map(_.event)
          .map {
            case e: OrderRetrying => e.copy(delayedUntil = None)
            case e => e
          } == Seq(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / try_(0) % 0),
          OrderStarted,

          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / catch_(0) % 0),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(toLocalSubagentId(agentPath)),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(0) / catch_(0) % 1),
          OrderRetrying(),

          OrderAwoke,
          OrderMoved(Position(0) / try_(1) % 0),
          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / catch_(1) % 0),
          OrderProcessingStarted(toLocalSubagentId(agentPath)),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(0) / catch_(1) % 1),
          OrderRetrying(),

          OrderAwoke,
          OrderMoved(Position(0) / try_(2) % 0),
          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / catch_(2) % 0),
          OrderProcessingStarted(toLocalSubagentId(agentPath)),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(0) / catch_(2) % 1),
          OrderRetrying()))

        controller.api
          .executeCommand(CancelOrders(Seq(orderId), CancellationMode.FreshOrStarted()))
          .await(99.s).orThrow
        eventWatch.await[OrderCancelled](_.key == orderId)
      }
  }

  "JS-2089 Cancel an Order waiting in Retry instruction at an Agent" in:
    val workflow = Workflow(WorkflowPath("RETRY"), Seq(
      TryInstruction(
        Workflow.of(
          FailingJob.execute(agentPath)),
        Workflow.of(
          Retry()),
        retryDelays = Some(Vector(100.s)))))

    withItem(workflow): workflow =>
      val orderId = OrderId("RETRY")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderRetrying](_.key == orderId)

      execCmd(SuspendOrders(Seq(orderId)))
      eventWatch.await[OrderDetached](_.key == orderId)
      //eventWatch.await[OrderSuspended](_.key == orderId)

      execCmd(CancelOrders(Seq(orderId)))
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId)
        .map {
          case OrderRetrying(Some(_), None) => OrderRetrying()
          case o => o
        } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(0) / "try+0" % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(0) / "catch+0" % 0),
        OrderRetrying(),

        OrderSuspensionMarked(),
        OrderDetachable,
        OrderDetached,
        OrderStateReset,
        OrderCancelled,
        OrderDeleted))

  "JS-2105 Cancel while retrying (Engine has to synchronize OrderDetachable with ongoing events)" in
    repeatTest(if isIntelliJIdea then 100 else 10) { testIndex =>
      // No more InapplicableEventProblem!
      val workflow = Workflow(WorkflowPath("CANCEL-WHILE-RETRYING"), Seq(
        TryInstruction(
          Workflow.of(
            FailingJob.execute(agentPath)),
          Workflow.of(
            Retry()))))

      withItem(workflow) { workflow =>
        val orderId = OrderId(s"🟨$testIndex")
        var eventId = eventWatch.lastAddedEventId
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path, deleteWhenTerminated = true))
        eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId
        sleep(Random.nextInt(10).ms)
        controller.api
          .executeCommand(CancelOrders(Seq(orderId), CancellationMode.FreshOrStarted()))
          .await(99.s)
          .orThrow
        eventWatch.await[OrderCancelled](_.key == orderId)

        assert(eventWatch
          .keyedEvents[OrderEvent](_.key == orderId, after = EventId.BeforeFirst)
          .take(9)
          .map(_.event)
          .map {
            case e: OrderRetrying => e.copy(delayedUntil = None)
            case e => e
          } == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(0) / try_(0) % 0),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(toLocalSubagentId(agentPath)),
          OrderProcessed(FailingJob.outcome),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderRetrying()))
      }
    }

  "JS-2117 Suspend and resume while retrying" in:
    // No more InapplicableEventProblem!
    val workflow = Workflow(WorkflowPath("SUSPEND-WHILE-RETRYING"), Seq(
      TryInstruction(
        Workflow.of(
          FailingJob.execute(agentPath)),
        Workflow.of(
          Retry()),
        retryDelays = Some(Vector(500.ms/*time-critical !!!*/)))))

    withItem(workflow) { workflow =>
      val orderId = OrderId("🟩")
      var eventId = eventWatch.lastAddedEventId

      controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path, deleteWhenTerminated = true))
      eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId

      /// Suspend, then resume before retry time as elapsed ///

      execCmd(SuspendOrders(Seq(orderId)))
      eventWatch.await[OrderSuspensionMarked](_.key == orderId, after = eventId)
      eventWatch.await[OrderDetached](_.key == orderId, after = eventId)

      execCmd(ResumeOrders(Seq(orderId)))
      eventWatch.await[OrderResumptionMarked](_.key == orderId)
      eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId

      /// Suspend, wait until retry time as elapsed, await OrderSuspended ///

      execCmd(SuspendOrders(Seq(orderId)))
      eventWatch.await[OrderSuspensionMarked](_.key == orderId, after = eventId)
      eventWatch.await[OrderDetached](_.key == orderId, after = eventId)
      eventWatch.await[OrderSuspended](_.key == orderId, after = eventId)

      execCmd(ResumeOrders(Seq(orderId)))
      eventWatch.await[OrderResumed](_.key == orderId)
      eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId

      /// Suspend, then cancel ///

      execCmd(SuspendOrders(Seq(orderId)))
      eventWatch.await[OrderSuspensionMarked](_.key == orderId, after = eventId)
      eventWatch.await[OrderDetached](_.key == orderId, after = eventId)

      controller
        .api.executeCommand(CancelOrders(Seq(orderId), CancellationMode.FreshOrStarted()))
        .await(99.s).orThrow
      eventWatch.await[OrderCancelled](_.key == orderId)
      eventWatch.await[OrderDeleted](_.key == orderId)

      assert(eventWatch
        .keyedEvents[OrderEvent](_.key == orderId, after = EventId.BeforeFirst)
        //.take(9)
        .map(_.event)
        .map {
          case e: OrderRetrying => e.copy(delayedUntil = None)
          case e => e
        } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(0) / try_(0) % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(toLocalSubagentId(agentPath)),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(0) / "catch+0" % 0),
        OrderRetrying(),

        OrderSuspensionMarked(),
        OrderDetachable,
        OrderDetached,

        OrderResumptionMarked(),
        OrderAwoke,
        OrderMoved(Position(0) / "try+1" % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(toLocalSubagentId(agentPath)),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(0) / "catch+1" % 0),
        OrderRetrying(),

        OrderSuspensionMarked(),
        OrderDetachable,
        OrderDetached,
        OrderAwoke,
        OrderMoved(Position(0) / "try+2" % 0),
        OrderSuspended,
        OrderResumed(),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(toLocalSubagentId(agentPath)),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(0) / "catch+2" % 0),
        OrderRetrying(),

        OrderSuspensionMarked(),
        OrderDetachable,
        OrderDetached,

        OrderStateReset,
        OrderCancelled,
        OrderDeleted))
    }

  "GoOrder when waiting in Retry at Controller" in:
    val workflow = Workflow.of(
      TryInstruction(
        Workflow.of(
          Fail()),
        Workflow.of(
          Retry()),
        retryDelays = Some(Vector(1.h)),
        maxTries = Some(2)))

    withItem(workflow) { workflow =>
      val orderId = OrderId("🐞")
      var eventId = eventWatch.lastAddedEventId
      controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path, deleteWhenTerminated = true))
      eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId

      // Order is not at the required Position
      locally:
        controller.api
          .executeCommand(GoOrder(orderId, position = Position(0)))
          .await(99.s)
        sleep(200.ms)
        assert(controllerState.idToOrder(orderId).isState[Order.DelayingRetry])

      locally:
        val checked = controller.api
          .executeCommand(GoOrder(orderId, position = Position(0) / "try+999" % 0))
          .await(99.s)
        assert(checked == Left(GoOrderInapplicableProblem(orderId)))

      val caughtPosition = Position(0) / catch_(0) % 0
      execCmd:
        GoOrder(orderId, position = caughtPosition)

      assert(eventWatch
        .keyedEvents[OrderEvent](_.key == orderId, after = EventId.BeforeFirst)
        .map(_.event)
        .map {
          case e: OrderRetrying => e.copy(delayedUntil = None)
          case e => e
        } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(0) / try_(0) % 0),
        OrderStarted,

        OrderOutcomeAdded(OrderOutcome.failed),
        OrderCaught(caughtPosition),
        OrderRetrying(),
        OrderGoes,
        OrderAwoke,
        OrderMoved(Position(0) / try_(1) % 0),
        OrderOutcomeAdded(OrderOutcome.failed),
        OrderFailed(Position(0) / try_(1) % 0)))
    }

  "GoOrder when waiting in Retry at Agent" in:
    val workflow = Workflow.of(
      TryInstruction(
        Workflow.empty,
        Workflow.of(
          Retry())),  // Only used for a wrong GoOrder Position
      TryInstruction(
        Workflow.of(
          FailingJob.execute(agentPath)),
        Workflow.of(
          Retry()),
        retryDelays = Some(Vector(1.h)),
        maxTries = Some(2)))

    withItem(workflow) { workflow =>
      val orderId = OrderId("🐌")
      var eventId = eventWatch.lastAddedEventId
      controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path, deleteWhenTerminated = true))
      eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).last.eventId

      val retryPosition = Position(1) / catch_(0) % 0

      assert(controllerState.idToOrder(orderId).isState[Order.DelayingRetry]
        && controllerState.idToOrder(orderId).position == retryPosition)

      // Position cannot be statically checked because OrderRetrying has moved
      // the Order to the try block.
      //val checked = controller.api
      //  .executeCommand(GoOrder(orderId, position = Position(0)))
      //  .await(99.s)
      //assert(checked == Left(GoOrderInapplicableProblem(orderId)))

      locally:
        assert(!controllerState.idToOrder(orderId).isGoCommandable(Position(0) / catch_(0) % 0))
        val checked = controller.api
          .executeCommand(GoOrder(orderId, position = Position(0) / catch_(0) % 0))
          .await(99.s)
        assert(checked == Left(GoOrderInapplicableProblem(orderId)))

      execCmd:
        GoOrder(orderId, position = retryPosition)
      eventWatch.await[OrderFailed](_.key == orderId)

      assert(eventWatch
        .keyedEvents[OrderEvent](_.key == orderId, after = EventId.BeforeFirst)
        .map(_.event)
        .map {
          case e: OrderRetrying => e.copy(delayedUntil = None)
          case e => e
        } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(1) / try_(0) % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(1) / catch_(0) % 0),
        OrderRetrying(),
        OrderGoMarked(Position(1) / catch_(0) % 0),
        OrderGoes,
        OrderAwoke,
        OrderMoved(Position(1) / try_(1) % 0),
        OrderProcessingStarted(subagentId),
        OrderProcessed(FailingJob.outcome),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1) / try_(1) % 0)))
    }

  "tryCount, maxTries" in :
    testTryCountAndMaxTries(expr("tryCount == maxTries"))

  "$js7TryCount, $js7MaxTries" in :
    testTryCountAndMaxTries(expr("$js7TryCount == $js7MaxTries"))

  private var nr = 0

  private def testTryCountAndMaxTries(expression: Expression): Assertion =
    nr += 1
    val workflow = Workflow.of(WorkflowPath(s"MAX-TRIES-$nr"),
      TryInstruction(
        tryWorkflow = Workflow.of(
          If(expression):
            Prompt(expr("'PROMPT'")),
          Fail()),
        catchWorkflow = Workflow.of:
          Retry(),
        maxTries = 2.some))

    withItem(workflow): workflow =>
      val expectedEvents = Vector(
        OrderAdded(workflow.id),
        OrderMoved(Position(0) / try_(0) % 1),
        OrderStarted,
        OrderOutcomeAdded(OrderOutcome.failed),

        OrderCaught(Position(0) / catch_(0) % 0),
        OrderRetrying(),
        OrderMoved(Position(0) / try_(1) % 0 / "then" % 0),
        OrderPrompted(StringValue("PROMPT")),
        OrderPromptAnswered(),
        OrderMoved(Position(0) / try_(1) % 0 / "then" % 1),
        OrderMoved(Position(0) / try_(1) % 1),
        OrderOutcomeAdded(OrderOutcome.failed),
        OrderFailed(Position(0) / try_(1) % 1))

      val orderId = OrderId(s"MAX-TRIES-$nr")
      val afterEventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.id.path)).await(99.s).orThrow
      eventWatch.awaitNext[OrderPrompted](_.key == orderId)
      execCmd(AnswerOrderPrompt(orderId))
      awaitAndCheckEventSeq[OrderTerminated](afterEventId, orderId, expectedEvents)


  private def awaitAndCheckEventSeq[E <: OrderEvent: ClassTag: Tag](
    after: EventId, orderId: OrderId, expected: Vector[OrderEvent])
  : Assertion =
    eventWatch.await[E](_.key == orderId, after = after)
    sleep(50.millis)  // No more events should arrive
    eventWatch.when[OrderEvent](EventRequest.singleClass(after = after, timeout = Some(0.s)))
      .await(99.s) match
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == expected)
      case o =>
        fail(s"Unexpected EventSeq received: $o")


object RetryTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
