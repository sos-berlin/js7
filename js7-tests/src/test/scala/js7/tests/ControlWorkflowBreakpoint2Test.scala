package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, ResumeOrder}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemAddedOrChanged, UnsignedItemChanged}
import js7.data.item.{ItemRevision, UnsignedItemEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderResumed, OrderStarted, OrderSuspended}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderObstacle, OrderObstacleCalculator, Outcome}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.WorkflowControlId.syntax._
import js7.data.workflow.instructions.{Prompt, TryInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPath}
import js7.data_for_java.controller.{JControllerCommand, JControllerState}
import js7.data_for_java.workflow.position.JPosition
import js7.data_for_java.workflow.{JWorkflowControl, JWorkflowControlId, JWorkflowId}
import js7.journal.watch.StrictEventWatch
import js7.proxy.ControllerApi
import js7.tests.ControlWorkflowBreakpoint2Test._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

final class ControlWorkflowBreakpoint2Test
extends AnyFreeSpec with ControllerAgentForScalaTest
{
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
  protected val items = Seq(aWorkflow, tryWorkflow)
  protected implicit def implicitEventWatch: StrictEventWatch = eventWatch
  protected implicit def implicitControllerApi: ControllerApi = controllerApi

  "Set some breakpoints" in {
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

    locally {
      val expectedWorkflowControl = WorkflowControl(
        aWorkflowControlId,
        Set(Position(0), Position(1)),
        itemRevision = Some(ItemRevision(0)))

      assert(controllerState.keyTo(WorkflowControl).toMap == Map(
        expectedWorkflowControl.id -> expectedWorkflowControl))

      assert(controllerState.keyToItem(WorkflowControl).toMap == Map(
        expectedWorkflowControl.id -> expectedWorkflowControl))

      assert(JControllerState(controllerState).idToWorkflowControl.asScala.toMap == Map(
        JWorkflowControlId(expectedWorkflowControl.id) -> JWorkflowControl(expectedWorkflowControl)))
    }

    val aOrderId = OrderId("A")

    def orderObstacles: Checked[Seq[(OrderId, Set[OrderObstacle])]] =
      new OrderObstacleCalculator(controllerState)
        .ordersToObstacles(Seq(aOrderId), Timestamp.now)
        .map(_.toSeq)

    controllerApi.addOrder(FreshOrder(aOrderId, aWorkflow.path, deleteWhenTerminated = true))
      .await(99.s)

    // Breakpoint at Position(0)
    eventWatch.await[OrderSuspended](_.key == aOrderId, after = eventId)
    assert(orderObstacles == Right(Seq(
      aOrderId -> Set[OrderObstacle](OrderObstacle.WaitingForCommand))))

    controllerApi.executeCommand(ResumeOrder(aOrderId)).await(99.s).orThrow
    eventId = eventWatch.await[OrderPrompted](_.key == aOrderId, after = eventId)
      .last.eventId

    controllerApi.executeCommand(AnswerOrderPrompt(aOrderId)).await(99.s).orThrow

    // Breakpoint at Position(1) reached
    eventWatch.await[OrderSuspended](_.key == aOrderId, after = eventId)

    eventId = setBreakpoints(
      aWorkflow.id,
      Map(Position(3) -> true),
      Set(Position(0), Position(1), Position(3)),
      ItemRevision(1),
      UnsignedItemChanged.apply)

    controllerApi.executeCommand(ResumeOrder(aOrderId)).await(99.s).orThrow

    // Breakpoint at Position(3) reached
    eventWatch.await[OrderSuspended](_.key == aOrderId, after = eventId)

    controllerApi.executeCommand(ResumeOrder(aOrderId)).await(99.s).orThrow
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
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended,
      OrderResumed(),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(4)),
      OrderDetachable,
      OrderDetached,
      OrderFinished,
      OrderDeleted))

    setBreakpoints(
      aWorkflow.id,
      Map(
        Position(1) -> false,
        Position(3) -> true),
      Set(Position(0), Position(3)),
      ItemRevision(2),
      UnsignedItemChanged.apply)

    waitForCondition(10.s, 10.ms)(
      controllerState.itemToIgnorantAgents(WorkflowControl).contains(aWorkflowControlId))
    assert(controllerState.itemToIgnorantAgents(WorkflowControl).toMap == Map(
      aWorkflowControlId -> Set(agentPath)))
    assert(controllerState.itemToIgnorantAgents(WorkflowControl)
      .get(aWorkflowControlId) contains Set(agentPath))
    assert(JControllerState(controllerState).workflowControlToIgnorantAgent
      .get(JWorkflowId(aWorkflowControlId.workflowId)).asScala == Set(agentPath))
  }
}

object ControlWorkflowBreakpoint2Test
{
  private val agentPath = AgentPath("A-AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL", Seq(
    Prompt(expr("'PROMPT'")),
    EmptyJob.execute(agentPath),
    EmptyJob.execute(agentPath),
    EmptyJob.execute(agentPath)))

  private val tryWorkflow = Workflow(WorkflowPath("TRY-WORKFLOW") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    TryInstruction(
      Workflow.of(EmptyJob.execute(agentPath)),
      Workflow.of(EmptyJob.execute(agentPath)))))

  private val aWorkflowControlId = WorkflowControlId(aWorkflow.id)

  private[tests] def setBreakpoints(
    workflowId: WorkflowId,
    breakpoints: Map[Position, Boolean],
    resultBreakpoints: Set[Position],
    revision: ItemRevision,
    workflowControlToEvent: WorkflowControl => UnsignedItemEvent)
    (implicit controllerApi: ControllerApi, eventWatch: StrictEventWatch)
  : EventId = {
    val eventId = eventWatch.lastAddedEventId
    val jCmd = JControllerCommand
      .controlWorkflow(
        JWorkflowId(workflowId),
        breakpoints.view
          .map { case (k, v) => JPosition(k) -> Boolean.box(v) }
          .toMap
          .asJava)
    controllerApi.executeCommand(jCmd.asScala).await(99.s).orThrow
    val keyedEvents = eventWatch.await[UnsignedItemAddedOrChanged](after = eventId)
    assert(keyedEvents.map(_.value) == Seq(
      NoKey <-: workflowControlToEvent(
        WorkflowControl(
          WorkflowControlId(workflowId),
          breakpoints = resultBreakpoints,
          itemRevision = Some(revision)))))
    keyedEvents.last.eventId
  }
}
