package js7.tests

import cats.effect.unsafe.IORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, ControlWorkflow, ResumeOrder}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemAddedOrChanged, UnsignedItemChanged}
import js7.data.item.{ItemRevision, UnsignedItemEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderResumed, OrderStarted, OrderSuspended}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderObstacle, OrderObstacleCalculator, OrderOutcome}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{If, Prompt, TryInstruction}
import js7.data.workflow.position.BranchId.{Try_, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath}
import js7.data_for_java.controller.{JControllerCommand, JControllerState}
import js7.data_for_java.workflow.position.JPosition
import js7.data_for_java.workflow.{JWorkflowControl, JWorkflowControlId, JWorkflowId}
import js7.journal.watch.StrictEventWatch
import js7.proxy.ControllerApi
import js7.tests.ControlWorkflowBreakpointTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import scala.jdk.CollectionConverters.*

final class ControlWorkflowBreakpointTest
extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    js7.controller.agent-driver.command-error-delay = 1s
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(aWorkflow)
  protected implicit def implicitEventWatch: StrictEventWatch = eventWatch
  protected implicit def implicitControllerApi: ControllerApi = controller.api

  "Set some breakpoints" in:
    var eventId = setBreakpoints(
      aWorkflow.id,
      Map(
        Position(0) -> true,
        Position(1) -> true),
      resultBreakpoints = Set(
        Position(0),
        Position(1)),
      revision = ItemRevision(0),
      UnsignedItemAdded.apply)

    assert(controllerState.itemToIgnorantAgents(WorkflowControl).isEmpty)
    assert(!controllerState.itemToIgnorantAgents(WorkflowControl).contains(aWorkflowControlId))

    locally:
      val expectedWorkflowControl = WorkflowControl(
        aWorkflowControlId,
        Set(Position(0), Position(1)),
        itemRevision = Some(ItemRevision(0)))

      assert(controllerState.keyTo(WorkflowControl).toMap == Map:
        expectedWorkflowControl.id -> expectedWorkflowControl)

      assert(controllerState.keyToItem(WorkflowControl).toMap == Map:
        expectedWorkflowControl.id -> expectedWorkflowControl)

      assert(JControllerState(controllerState).idToWorkflowControl.asScala.toMap == Map:
        JWorkflowControlId(expectedWorkflowControl.id) -> JWorkflowControl(expectedWorkflowControl))

    val aOrderId = OrderId("A")

    def orderObstacles: Checked[Seq[(OrderId, Set[OrderObstacle])]] =
      new OrderObstacleCalculator(controllerState)
        .ordersToObstacles(Seq(aOrderId), Timestamp.now)
        .map(_.toSeq)

    controller.api.addOrder(FreshOrder(aOrderId, aWorkflow.path, deleteWhenTerminated = true))
      .await(99.s)

    // Breakpoint at Position(0)
    eventWatch.await[OrderSuspended](_.key == aOrderId, after = eventId)
    assert(orderObstacles == Right(Seq(
      aOrderId -> Set[OrderObstacle](OrderObstacle.WaitingForCommand))))

    controller.api.executeCommand(ResumeOrder(aOrderId)).await(99.s).orThrow
    eventId = eventWatch.await[OrderPrompted](_.key == aOrderId, after = eventId)
      .last.eventId

    controller.api.executeCommand(AnswerOrderPrompt(aOrderId)).await(99.s).orThrow

    // Breakpoint at Position(1) reached
    eventWatch.await[OrderSuspended](_.key == aOrderId, after = eventId)

    eventId = setBreakpoints(
      aWorkflow.id,
      Map(Position(3) -> true),
      Set(Position(0), Position(1), Position(3)),
      ItemRevision(1),
      UnsignedItemChanged.apply)

    controller.api.executeCommand(ResumeOrder(aOrderId)).await(99.s).orThrow

    // Breakpoint at Position(3) reached
    eventWatch.await[OrderSuspended](_.key == aOrderId, after = eventId)

    controller.api.executeCommand(ResumeOrder(aOrderId)).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

    assert(eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
      OrderAdded(aWorkflow.id, deleteWhenTerminated = true),
      OrderSuspended,
      OrderResumed(),
      OrderStarted,
      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(1)),
      OrderSuspended,
      OrderResumed(),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended,
      OrderResumed(),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(4)),
      OrderDetachable,
      OrderDetached,
      OrderFinished(),
      OrderDeleted))

    setBreakpoints(
      aWorkflow.id,
      Map(
        Position(1) -> false,
        Position(3) -> true),
      Set(Position(0), Position(3)),
      ItemRevision(2),
      UnsignedItemChanged.apply)

    // TODO Unreliable, because WorkflowControl may have been attached to the Agent:
    //waitForCondition(10.s, 10.ms)(
    //  controllerState.itemToIgnorantAgents(WorkflowControl).contains(aWorkflowControlId))
    //assert(controllerState.itemToIgnorantAgents(WorkflowControl).toMap == Map(
    //  aWorkflowControlId -> Set(agentPath)))
    //assert(JControllerState(controllerState).workflowControlToIgnorantAgent
    //  .get(JWorkflowId(aWorkflowControlId.workflowId)).asScala == Set(agentPath))

  "Breakpoint in a Try block" in:
    val workflow = Workflow(WorkflowPath("IN-TRY-WORKFLOW"), Seq(
      EmptyJob.execute(agentPath),
      TryInstruction(
        Workflow.of(EmptyJob.execute(agentPath)),
        Workflow.of(EmptyJob.execute(agentPath)))))

    withItem(workflow): workflow =>
      val orderId = OrderId("🟦")
      controller.api
        .executeCommand(ControlWorkflow(workflow.id, addBreakpoints = Set(
          Position(1) / Try_ % 0)))
        .await(99.s).orThrow

      controller.api.addOrder(FreshOrder(orderId, workflow.id.path)).await(99.s).orThrow
      eventWatch.await[OrderSuspended](_.key == orderId)

      assert(controllerState.idToOrder(orderId).position == Position(1) / try_(0) % 0)

      controller.api.executeCommand(ResumeOrder(orderId)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == orderId)

  "Breakpoint at a Try block" in:
    val workflow = Workflow(WorkflowPath("AT-TRY-WORKFLOW"), Seq(
      EmptyJob.execute(agentPath),
      TryInstruction(
        Workflow.of(EmptyJob.execute(agentPath)),
        Workflow.of(EmptyJob.execute(agentPath)))))

    withItem(workflow): workflow =>
      val orderId = OrderId("🟪")
      controller.api
        .executeCommand(ControlWorkflow(workflow.id, addBreakpoints = Set(
          Position(1))))
        .await(99.s).orThrow

      controller.api.addOrder(FreshOrder(orderId, workflow.id.path)).await(99.s).orThrow
      eventWatch.await[OrderSuspended](_.key == orderId)

      assert(controllerState.idToOrder(orderId).position == Position(1))

      controller.api.executeCommand(ResumeOrder(orderId)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == orderId)

  "Breakpoint at an If instruction" in:
    val workflow = Workflow.of(WorkflowPath("IF-WORKFLOW"),
      If(expr("true")):
        If(expr("true")):
          Workflow.of(
            EmptyJob.execute(agentPath),
            If(expr("true")):
              Workflow.empty))

    val positions = Seq(
      Position(0),
      Position(0) / "then" % 0,
      Position(0) / "then" % 0 / "then" % 0,
      Position(0) / "then" % 0 / "then" % 1)

    withItem(workflow): workflow =>
      val orderId = OrderId("🟩")
      controller.api
        .executeCommand(ControlWorkflow(workflow.id, addBreakpoints = positions.toSet))
        .await(99.s).orThrow

      var eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.id.path, deleteWhenTerminated = true))
        .await(99.s).orThrow

      for position <- positions do
        eventId = eventWatch.await[OrderSuspended](_.key == orderId, after = eventId).last.eventId
        assert(controllerState.idToOrder(orderId).position == position)
        controller.api.executeCommand(ResumeOrder(orderId)).await(99.s).orThrow

      eventWatch.await[OrderFinished](_.key == orderId)


object ControlWorkflowBreakpointTest:
  private val agentPath = AgentPath("A-AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL", Seq(
    Prompt(expr("'PROMPT'")),
    EmptyJob.execute(agentPath),
    EmptyJob.execute(agentPath),
    EmptyJob.execute(agentPath)))

  private val aWorkflowControlId = WorkflowControlId(aWorkflow.id)

  private[tests] def setBreakpoints(
    workflowId: WorkflowId,
    breakpoints: Map[Position, Boolean],
    resultBreakpoints: Set[Position],
    revision: ItemRevision,
    workflowControlToEvent: WorkflowControl => UnsignedItemEvent)
    (using controllerApi: ControllerApi, eventWatch: StrictEventWatch, ioRuntime: IORuntime)
  : EventId =
    val eventId = eventWatch.lastAddedEventId
    val jCmd = JControllerCommand.controlWorkflow(
      JWorkflowId(workflowId),
      breakpoints.view
        .map((k, v) => JPosition(k) -> Boolean.box(v))
        .toMap
        .asJava)
    controllerApi.executeCommand(jCmd.asScala).await(99.s).orThrow
    val keyedEvents = eventWatch.await[UnsignedItemAddedOrChanged](after = eventId)
    assert(keyedEvents.map(_.value) == Seq:
      NoKey <-: workflowControlToEvent:
        WorkflowControl(
          WorkflowControlId(workflowId),
          breakpoints = resultBreakpoints,
          itemRevision = Some(revision)))
    keyedEvents.last.eventId
