package js7.data.workflow

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder}
import js7.base.circeutils.typed.Subtype
import js7.base.problem.Checked
import js7.data.agent.AgentPath
import js7.data.workflow.WorkflowPathControlEvent.{WorkflowPathControlAttached, WorkflowPathControlUpdated}

final case class WorkflowPathControlState(
  workflowPathControl: WorkflowPathControl,
  attachedToAgents: Set[AgentPath] = Set.empty)
{
  def workflowPath: WorkflowPath =
    workflowPathControl.path

  def applyEvent(event: WorkflowPathControlEvent): Checked[WorkflowPathControlState] =
    event match {
      case WorkflowPathControlUpdated(suspended, skip, revision) =>
        Right(copy(
          workflowPathControl.copy(
            suspended = suspended,
            skip = skip,
            revision = revision),
          attachedToAgents = Set.empty))

      case WorkflowPathControlAttached(agentPath, revision) =>
        Right(
          if (revision != workflowPathControl.revision)
            this // Ignore late event
          else
            copy(
              attachedToAgents = attachedToAgents + agentPath))
    }
}

object WorkflowPathControlState
{
  private implicit val jsonDecoder: Decoder[WorkflowPathControlState] =
    c => for {
      workflowPathControl <- c.get[WorkflowPathControl](
        // COMPATIBLE with 2.4.0-beta.20220621
        if (c.value.asObject.exists(_ contains "workflowControl")) "workflowControl"
        else "workflowPathControl")
      attachedToAgents <- c.get[Set[AgentPath]]("attachedToAgents")
    } yield WorkflowPathControlState(workflowPathControl, attachedToAgents)

  val subtype = Subtype(
    Codec.AsObject.from(jsonDecoder, deriveCodec[WorkflowPathControlState]),
    aliases = Seq("WorkflowControlState" /*COMPATIBLE WITH 2.4.0-beta.20220621*/))
}
