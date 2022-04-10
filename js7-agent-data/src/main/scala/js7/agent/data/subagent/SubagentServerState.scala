package js7.agent.data.subagent

import js7.data.event.JournaledState
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.workflow.{Workflow, WorkflowId}

trait SubagentServerState[S <: SubagentServerState[S]]
extends JournaledState[S]
{
  this: S =>

  def idToWorkflow: Map[WorkflowId, Workflow]
  def pathToJobResource: Map[JobResourcePath, JobResource]
}
