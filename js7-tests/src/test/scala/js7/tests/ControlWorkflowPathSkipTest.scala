package js7.tests

import cats.syntax.option.*
import fs2.Stream
import java.time.ZoneId
import js7.agent.RunningAgent
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.{TestAlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, ControlWorkflowPath, ResumeOrder}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.ItemDetached
import js7.data.item.ItemOperation.{AddVersion, RemoveVersioned}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemAddedOrChanged}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.OrderEvent.OrderMoved.SkippedDueToWorkflowPathControl
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderPrompted, OrderResumed, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Cycle, EmptyInstruction, Execute, Fail, If, Prompt, Retry, Schedule, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{Label, Position}
import js7.data.workflow.{Workflow, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.ControlWorkflowPathSkipTest.*
import js7.tests.jobs.{EmptyJob, FailingJob}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.ControllerAgentForScalaTest

final class ControlWorkflowPathSkipTest
extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(aWorkflow, bWorkflow, calendar)

  private val clock =
    given ZoneId = ZoneId.of("Europe/Stockholm")
    TestAlarmClock(local("2024-07-06T12:00"))

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    alarmClock = Some(clock))


  "Skip the only Job" in:
    val orderId = OrderId("A")
    skipInstruction(aWorkflow.path, true, ItemRevision(1))
    val events = controller
      .runOrder(FreshOrder(orderId, aWorkflow.path, deleteWhenTerminated = true))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(aWorkflow.id, deleteWhenTerminated = true),
      // skipped
      OrderMoved(Position(1), reason = Some(OrderMoved.SkippedDueToWorkflowPathControl)),
      OrderStarted,
      OrderFinished(),
      OrderDeleted))

  "Skip the Job in the middle" in:
    skipInstruction(bWorkflow.path, true, ItemRevision(1))
    val orderId = OrderId("B")
    val events = controller
      .runOrder(FreshOrder(orderId, bWorkflow.path, deleteWhenTerminated = true))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(bWorkflow.id, deleteWhenTerminated = true),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),

      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1) / "then" % 0),
      OrderMoved(Position(1) / "then" % 1, reason = Some(OrderMoved.SkippedDueToWorkflowPathControl)),
      OrderMoved(Position(2)),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(3)),

      OrderDetachable,
      OrderDetached,
      OrderFinished(),
      OrderDeleted))

  "JS-2103 Bug: frozen Orders and \"Unhandled message StartProcessing\"" in:
    val workflow = Workflow(WorkflowPath("JS-2103-WORKFLOW"), Seq(
      EmptyJob.execute(agentPath),
      If(expr("true")):
        label @: Execute(WorkflowJob.Name("JOB")),
      EmptyJob.execute(agentPath),
      // JS-2103 bug blocks here
      Execute(WorkflowJob.Name("JOB"))),
      nameToJob = Map(
        WorkflowJob.Name("JOB") -> EmptyJob.workflowJob(agentPath)))

    withItem(workflow): workflow =>
      skipInstruction(workflow.path, true, ItemRevision(1))
      val orderId = OrderId("B")
      val events = controller
        .runOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .map(_.value)
      assert(events == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),

        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1) / "then" % 0),
        OrderMoved(Position(1) / "then" % 1,
          reason = Some(OrderMoved.SkippedDueToWorkflowPathControl)),
        OrderMoved(Position(2)),

        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(3)),

        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(4)),

        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))

  "JS-2134 skip is lazy" in:
    eventWatch.resetLastWatchedEventId()
    val workflow = Workflow(WorkflowPath("LAZY"), Seq(
      label @: EmptyJob.execute(agentPath)))
    withItem(workflow): workflow =>
      skipInstruction(workflow.path, true, ItemRevision(1))
      val orderId = OrderId("LAZY")
      val scheduledFor = clock.now() + 1.s
      controller
        .addOrderBlocking(FreshOrder(orderId, workflow.path, scheduledFor = Some(scheduledFor),
          deleteWhenTerminated = true))
      eventWatch.awaitNext[OrderAttached](_.key == orderId)

      clock += 1.s
      eventWatch.awaitNext[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, scheduledFor = Some(scheduledFor), deleteWhenTerminated = true),
        // skipped
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderMoved(Position(1), reason = Some(OrderMoved.SkippedDueToWorkflowPathControl)),
        OrderDetachable,
        OrderDetached,
        OrderStarted,
        OrderFinished(),
        OrderDeleted))


  "WorkflowPathControl disappears with the last Workflow version" in:
    val eventId = eventWatch.lastAddedEventId

    controller.api
      .updateItems(Stream(
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(aWorkflow.path),
        RemoveVersioned(bWorkflow.path)))
      .await(99.s)
    assert(eventWatch.await[ItemDetached](_.event.key == bWorkflow.id, after = eventId)
      .head.value.event
      == ItemDetached(bWorkflow.id, agentPath))

    assert(agent.currentAgentState().keyTo(WorkflowPathControl).isEmpty)
    awaitAndAssert:
      controllerState.keyTo(WorkflowPathControl).isEmpty

  "JS-2132 Skip a statement when an Order has failed" in:
    val workflow = Workflow(WorkflowPath("JS-2132-WORKFLOW"), Seq(
      label @: FailingJob.execute(agentPath)))

    withItem(workflow): workflow =>
      val orderId = OrderId("JS-2132")
      val events = controller
        .runOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .map(_.value)
      assert(events == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),

        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(FailingJob.outcome),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))))

      val eventId = eventWatch.lastAddedEventId
      // The Order must not be moved due to skip, because it has failed
      skipInstruction(workflow.path, true, ItemRevision(1))

      controller.api
        .executeCommand:
          ResumeOrder(orderId, asSucceeded = true)
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](after = eventId)
      assert(eventWatch.eventsByKey[OrderEvent](orderId, after = eventId) == Seq(
        OrderResumed(asSucceeded = true),
        OrderMoved(Position(1), Some(SkippedDueToWorkflowPathControl)),
        OrderFinished(),
        OrderDeleted))

  private def skipInstruction(workflowPath: WorkflowPath, skip: Boolean, revision: ItemRevision)
  : EventId =
    val eventId = eventWatch.lastAddedEventId
    skipInstruction(workflowPath, skip)
    val keyedEvents = eventWatch.await[UnsignedSimpleItemAddedOrChanged](after = eventId)
    assert(keyedEvents.map(_.value) == Seq(
      NoKey <-: UnsignedSimpleItemAdded(
        WorkflowPathControl(
          WorkflowPathControlPath(workflowPath),
          skip = Set(label).filter(_ => skip),
          itemRevision = Some(revision)))))
    keyedEvents.last.eventId

  private def skipInstruction(workflowPath: WorkflowPath, skip: Boolean = true): Unit =
    controller.api
      .executeCommand(ControlWorkflowPath(workflowPath, skip = Map(
        label -> skip)))
      .await(99.s).orThrow

  "Skip any instruction (JS-2112)" - {
    "Skip first instruction" in:
      val workflow = Workflow.of(WorkflowPath("SKIP-FIRST-INSTRUCTION"),
        label @:
          If(expr("true")):
            EmptyJob.execute(agentPath),
        EmptyInstruction())

      withItem(workflow): workflow =>
        skipInstruction(workflow.path)
        val orderId = OrderId("SKIP-FIRST-INSTRUCTION")
        val events = controller
          .runOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .map(_.value)
        assert(events == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(1), Some(OrderMoved.SkippedDueToWorkflowPathControl)),
          OrderMoved(Position(2)),
          OrderStarted,
          OrderFinished(),
          OrderDeleted))

    "Skip instruction after Execute" in:
      val workflow = Workflow.of(WorkflowPath("SKIP-AFTER-EXECUTE"),
        EmptyJob.execute(agentPath),
        label @:
          If(expr("true")):
            EmptyInstruction())

      withItem(workflow): workflow =>
        skipInstruction(workflow.path)
        val orderId = OrderId("SKIP-AFTER-EXECUTE")
        val events = controller
          .runOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .map(_.value)
        assert(events == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(1)),
          OrderMoved(Position(2), Some(OrderMoved.SkippedDueToWorkflowPathControl)),
          OrderDetachable,
          OrderDetached,
          OrderFinished(),
          OrderDeleted))

    "Cycle is skipped only before entering it" in:
      val workflow = Workflow(WorkflowPath("SKIP-CYCLE"),
        Seq(
          label @:
            Cycle(Schedule.continuous(0.s, limit = 2.some)):
              Workflow.of:
                Prompt(expr("'PROMPT'"))),
        calendarPath = calendar.path.some)

      withItem(workflow): workflow =>
        eventWatch.resetLastWatchedEventId()
        val orderId = OrderId("#2024-04-10#SKIP-CYCLE")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)

        eventWatch.awaitNext[OrderPrompted](_.key == orderId)
        skipInstruction(workflow.path)
        controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow

        eventWatch.awaitNext[OrderPrompted](_.key == orderId)
        controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow

        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

    "Try with retry is skipped only before entering it" in:
      val workflow = Workflow(WorkflowPath("SKIP-TRY"),
        Seq(
          label @:
            TryInstruction(
              Workflow.of(
                Prompt(expr("'PROMPT'")),
                Fail(None)),
              Workflow.of(
                Retry()),
              maxTries = 2.some)))

      withItem(workflow): workflow =>
        eventWatch.resetLastWatchedEventId()
        val orderId = OrderId("SKIP-TRY")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)

        eventWatch.awaitNext[OrderPrompted](_.key == orderId)
        skipInstruction(workflow.path)
        controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow

        eventWatch.awaitNext[OrderPrompted](_.key == orderId)
        controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow

        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

    "JS-2132 Skip a statement when an Order has been failed" in:
      val workflow = Workflow(WorkflowPath("JS-2132-WORKFLOW"), Seq(
        label @: FailingJob.execute(agentPath)))

      withItem(workflow): workflow =>
        val orderId = OrderId("JS-2132")
        val events = controller
          .runOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .map(_.value)
        assert(events == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),

          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(FailingJob.outcome),
          OrderDetachable,
          OrderDetached,
          OrderFailed(Position(0))))

        val eventId = eventWatch.lastAddedEventId
        // The Order must not be moved due to skip, because it has failed
        skipInstruction(workflow.path, true, ItemRevision(1))

        controller.api
          .executeCommand(
            ResumeOrder(orderId, asSucceeded = true))
          .await(99.s).orThrow
        eventWatch.await[OrderTerminated](after = eventId)
        assert(eventWatch.eventsByKey[OrderEvent](orderId, after = eventId) == Seq(
          OrderResumed(asSucceeded = true),
          OrderMoved(Position(1), Some(SkippedDueToWorkflowPathControl)),
          OrderFinished(),
          OrderDeleted))
  }


object ControlWorkflowPathSkipTest:

  private val agentPath = AgentPath("A-AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val calendar = Calendar.jocStandard(CalendarPath("CALENDAR"))

  private val label = Label("SKIP")
  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ VersionId("INITIAL"), Seq(
    label @: EmptyJob.execute(agentPath)))

  private val bWorkflow = Workflow(WorkflowPath("B-WORKFLOW") ~ VersionId("INITIAL"), Seq(
    EmptyJob.execute(agentPath),
    If(expr("true")):
      label @: EmptyJob.execute(agentPath),
    EmptyJob.execute(agentPath)))
