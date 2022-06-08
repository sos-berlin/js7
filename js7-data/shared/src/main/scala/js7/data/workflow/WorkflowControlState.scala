package js7.data.workflow

import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.Checked
import js7.data.agent.AgentPath
import js7.data.workflow.WorkflowControlEvent.{WorkflowControlAttached, WorkflowControlUpdated}

final case class WorkflowControlState(
  workflowControl: WorkflowControl,
  attachedToAgents: Set[AgentPath] = Set.empty)
{
  def workflowPath: WorkflowPath =
    workflowControl.path

  def applyEvent(event: WorkflowControlEvent): Checked[WorkflowControlState] =
    event match {
      case WorkflowControlUpdated(suspended, revision) =>
        Right(copy(
          workflowControl.copy(
            suspended = suspended,
            revision = revision),
          attachedToAgents = Set.empty))

      case WorkflowControlAttached(agentPath, suspended, revision) =>
        Right(
          if (revision != workflowControl.revision)
            this // Ignore late event
          else
            copy(
              attachedToAgents = attachedToAgents + agentPath))
    }
}

object WorkflowControlState {
  implicit val jsonCodec = deriveCodec[WorkflowControlState]
}
