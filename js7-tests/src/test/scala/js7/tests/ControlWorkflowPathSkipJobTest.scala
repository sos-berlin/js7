package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.Test
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.ControlWorkflowPath
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.ItemDetached
import js7.data.item.ItemOperation.{AddVersion, RemoveVersioned}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemAddedOrChanged}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.If
import js7.data.workflow.position.{Label, Position}
import js7.data.workflow.{Workflow, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import js7.tests.ControlWorkflowPathSkipJobTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable

final class ControlWorkflowPathSkipJobTest
extends Test with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(aWorkflow, bWorkflow)

  "Skip the only Job" in {
    val orderId = OrderId("A")
    skipJob(aWorkflow.path, true, ItemRevision(1))
    val events = controller
      .runOrder(FreshOrder(orderId, aWorkflow.path, deleteWhenTerminated = true))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(aWorkflow.id, deleteWhenTerminated = true),
      // skipped
      OrderMoved(Position(1)),
      OrderStarted,
      OrderFinished,
      OrderDeleted))
  }

  "Skip the Job in the middle" in {
    skipJob(bWorkflow.path, true, ItemRevision(1))
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
      OrderProcessed(Outcome.succeeded),
      // skipped
      OrderMoved(Position(2)),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(3)),

      OrderDetachable,
      OrderDetached,
      OrderFinished,
      OrderDeleted))
  }

  "WorkflowPathControl disappears with the last Workflow version" in {
    val controllerApi = directoryProvider.newControllerApi(controller)
    val eventId = eventWatch.lastAddedEventId

    controllerApi
      .updateItems(Observable(
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(aWorkflow.path),
        RemoveVersioned(bWorkflow.path)))
      .await(99.s)
    assert(eventWatch.await[ItemDetached](_.event.key == bWorkflow.id, after = eventId)
      .head.value.event
      == ItemDetached(bWorkflow.id, agentPath))

    assert(agent.currentAgentState().keyTo(WorkflowPathControl).isEmpty)
    assert(controller.controllerState.await(99.s).keyTo(WorkflowPathControl).isEmpty)
  }

  private def skipJob(workflowPath: WorkflowPath, skip: Boolean, revision: ItemRevision)
  : EventId = {
    val eventId = eventWatch.lastAddedEventId
    controllerApi
      .executeCommand(ControlWorkflowPath(workflowPath, skip = Map(
        label -> skip)))
      .await(99.s).orThrow
    val keyedEvents = eventWatch.await[UnsignedSimpleItemAddedOrChanged](after = eventId)
    assert(keyedEvents.map(_.value) == Seq(
      NoKey <-: UnsignedSimpleItemAdded(
        WorkflowPathControl(
          WorkflowPathControlPath(workflowPath),
          skip = Set(label).filter(_ => skip),
          itemRevision = Some(revision)))))
    keyedEvents.last.eventId
  }
}

object ControlWorkflowPathSkipJobTest
{
  private val agentPath = AgentPath("A-AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val label = Label("SKIP")
  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ VersionId("INITIAL"), Seq(
    label @: EmptyJob.execute(agentPath)))

  private val bWorkflow = Workflow(WorkflowPath("B-WORKFLOW") ~ VersionId("INITIAL"), Seq(
    EmptyJob.execute(agentPath),
    If(expr("true"), Workflow.of(
      label @: EmptyJob.execute(agentPath))),
    EmptyJob.execute(agentPath)))
}
