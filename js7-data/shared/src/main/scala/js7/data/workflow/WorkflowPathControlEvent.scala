package js7.data.workflow

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentPath
import js7.data.event.Event
import js7.data.item.ItemRevision

sealed trait WorkflowPathControlEvent extends Event.ForScala3[WorkflowPathControlEvent]
{
  val companion = WorkflowPathControlEvent
}

object WorkflowPathControlEvent extends Event.Companion[WorkflowPathControlEvent]
{
  type Key = WorkflowPath

  final case class WorkflowPathControlUpdated(suspended: Boolean, revision: ItemRevision)
  extends WorkflowPathControlEvent

  /**
   * @param revision if != WorkflowPathControl.revision, then this event is informational only.
   * @param suspended informational only
   */
  final case class WorkflowPathControlAttached(
    agentPath: AgentPath,
    suspended: Boolean,
    revision: ItemRevision)
  extends WorkflowPathControlEvent

  implicit val jsonCodec = TypedJsonCodec[WorkflowPathControlEvent](
    Subtype(deriveCodec[WorkflowPathControlUpdated],
      aliases = Seq("WorkflowControlUpdated"/*COMPATIBLE WITH 2.4.0-beta.20220621*/)),
    Subtype(deriveCodec[WorkflowPathControlAttached],
      aliases = Seq("WorkflowControlAttached"/*COMPATIBLE WITH 2.4.0-beta.20220621*/)))
}
