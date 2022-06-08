package js7.data.workflow

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentPath
import js7.data.event.Event
import js7.data.item.ItemRevision

sealed trait WorkflowControlEvent extends Event.ForScala3[WorkflowControlEvent]
{
  val companion = WorkflowControlEvent
}

object WorkflowControlEvent extends Event.Companion[WorkflowControlEvent]
{
  type Key = WorkflowPath

  final case class WorkflowControlUpdated(suspended: Boolean, revision: ItemRevision)
  extends WorkflowControlEvent

  /**
   * @param revision if != WorkflowControl.revision, then this event is informational only.
   * @param suspended informational only
   */
  final case class WorkflowControlAttached(
    agentPath: AgentPath,
    suspended: Boolean,
    revision: ItemRevision)
  extends WorkflowControlEvent

  implicit val jsonCodec = TypedJsonCodec[WorkflowControlEvent](
    Subtype(deriveCodec[WorkflowControlUpdated]),
    Subtype(deriveCodec[WorkflowControlAttached]))
}
