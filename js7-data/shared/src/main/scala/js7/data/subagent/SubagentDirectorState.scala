package js7.data.subagent

import js7.base.crypt.Signed
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.SignableItem
import js7.data.job.{JobKey, JobResource, JobResourcePath}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.MapView

trait SubagentDirectorState[S <: SubagentDirectorState[S]]
extends SubagentDriverState[S]
{
  this: S =>

  def idToOrder: Map[OrderId, Order[Order.State]]
  def idToWorkflow: Map[WorkflowId, Workflow]
  def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob]
  def idToSubagentItemState: MapView[SubagentId, SubagentItemState]
  def pathToJobResource: Map[JobResourcePath, JobResource]
  def keyToSigned[I <: SignableItem](I: SignableItem.Companion[I]): MapView[I.Key, Signed[I]]

  final def jobKey(workflowPosition: WorkflowPosition): Checked[JobKey] =
    for {
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      jobKey <- workflow.positionToJobKey(workflowPosition.position)
    } yield jobKey
}
