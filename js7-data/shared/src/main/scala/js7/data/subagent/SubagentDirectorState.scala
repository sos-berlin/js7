package js7.data.subagent

import js7.base.crypt.Signed
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentRunId
import js7.data.event.JournaledState
import js7.data.item.{SignableItem, UnsignedSimpleItem}
import js7.data.job.{JobKey, JobResource, JobResourcePath}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.value.expression.Scope
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.MapView

trait SubagentDirectorState[S <: SubagentDirectorState[S]]
extends JournaledState[S]:
  this: S =>

  def agentRunId: AgentRunId

  def idToOrder: Map[OrderId, Order[Order.State]]

  def idToWorkflow: Map[WorkflowId, Workflow]

  def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob]

  def idToSubagentItemState: MapView[SubagentId, SubagentItemState]

  def pathToUnsignedSimple[A <: UnsignedSimpleItem](A: UnsignedSimpleItem.Companion[A])
  : MapView[A.Path, A]

  def pathToJobResource: Map[JobResourcePath, JobResource]

  def keyToSigned[I <: SignableItem](I: SignableItem.Companion[I]): MapView[I.Key, Signed[I]]

  def toOrderScope(order: Order[Order.State]): Checked[Scope]

  final def maybeJobKey(workflowPosition: WorkflowPosition): Option[JobKey] =
    for
      workflow <- idToWorkflow.get(workflowPosition.workflowId)
      jobKey <- workflow.positionToJobKeyMaybe(workflowPosition.position)
    yield
      jobKey

  final def jobKey(workflowPosition: WorkflowPosition): Checked[JobKey] =
    for
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      jobKey <- workflow.positionToJobKey(workflowPosition.position)
    yield
      jobKey

  /** Returns ProcessLost if job isRestartable, otherwise returns Disrupted. */
  final def orderProcessLostIfRestartable(order: Order[Order.State], problem: Problem): OrderProcessed =
    OrderProcessed:
      if workflowJob(order.workflowPosition).exists(_.isRestartable) then
        // ProcessLost indicates that we should restart the order process
        OrderOutcome.processLost(problem)
      else
        OrderOutcome.Disrupted(problem)
