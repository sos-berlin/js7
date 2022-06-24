package js7.data.workflow

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentPath
import js7.data.event.Event
import js7.data.item.ItemRevision
import js7.data.workflow.position.Label

sealed trait WorkflowPathControlEvent extends Event.ForScala3[WorkflowPathControlEvent]
{
  val companion = WorkflowPathControlEvent
}

object WorkflowPathControlEvent extends Event.Companion[WorkflowPathControlEvent]
{
  type Key = WorkflowPath

  final case class WorkflowPathControlUpdated(
    suspended: Boolean,
    skip: Set[Label] = Set.empty/*COMPATIBLE WITH 2.4.0-beta.20220621*/,
    revision: ItemRevision)
  extends WorkflowPathControlEvent

  /**
   * @param revision if != WorkflowPathControl.revision, then this event is informational only.
   */
  final case class WorkflowPathControlAttached(
    agentPath: AgentPath,
    revision: ItemRevision)
  extends WorkflowPathControlEvent

  implicit val jsonCodec = {
    implicit val x = withDefaults
    TypedJsonCodec[WorkflowPathControlEvent](
      Subtype(deriveConfiguredCodec[WorkflowPathControlUpdated],
        aliases = Seq("WorkflowControlUpdated"/*COMPATIBLE WITH 2.4.0-beta.20220621*/)),
      Subtype(deriveConfiguredCodec[WorkflowPathControlAttached],
        aliases = Seq("WorkflowControlAttached"/*COMPATIBLE WITH 2.4.0-beta.20220621*/)))
  }
}
