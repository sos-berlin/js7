package js7.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.web.Uri
import js7.data.item.SimpleItemEvent

sealed trait AgentRefEvent extends SimpleItemEvent
{
  type Key = AgentId
}

object AgentRefEvent
{
  sealed trait AgentAddedOrUpdated extends AgentRefEvent {
    def uri: Uri
  }
  final case class AgentAdded(uri: Uri) extends AgentAddedOrUpdated

  final case class AgentUpdated(uri: Uri) extends AgentAddedOrUpdated

  //case object AgentDeleted extends AgentRefEvent

  implicit val jsonCodec = TypedJsonCodec[AgentRefEvent](
    Subtype(deriveCodec[AgentAdded]),
    Subtype(deriveCodec[AgentUpdated]))
}
