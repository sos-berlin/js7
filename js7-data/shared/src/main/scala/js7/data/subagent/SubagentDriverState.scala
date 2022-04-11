package js7.data.subagent

import js7.data.event.JournaledState
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.workflow.{Workflow, WorkflowId}

trait SubagentDriverState[S <: SubagentDriverState[S]]
extends JournaledState[S]
{
  this: S =>

  def idToWorkflow: Map[WorkflowId, Workflow]
  def pathToJobResource: Map[JobResourcePath, JobResource]
}
