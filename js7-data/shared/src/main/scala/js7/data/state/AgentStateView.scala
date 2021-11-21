package js7.data.state

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{JobKey, JobResource, JobResourcePath}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Workflow, WorkflowId}

/** Common interface for ControllerState, AgentState and SubagentState. */
trait AgentStateView
{
  def idToWorkflow: PartialFunction[WorkflowId, Workflow]

  def pathToJobResource: PartialFunction[JobResourcePath, JobResource]

  final def jobKey(workflowPosition: WorkflowPosition): Checked[JobKey] =
    for {
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      jobKey <- workflow.positionToJobKey(workflowPosition.position)
    } yield jobKey

  final def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob] =
    for {
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      job <- workflow.checkedWorkflowJob(workflowPosition.position)
    } yield job

  private def keyToJob(jobKey: JobKey): Checked[WorkflowJob] =
    idToWorkflow.checked(jobKey.workflowId)
      .flatMap(_.keyToJob.checked(jobKey))
}
