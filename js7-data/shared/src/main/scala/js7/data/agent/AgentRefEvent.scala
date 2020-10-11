package js7.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.web.Uri
import js7.data.event.Event

sealed trait AgentRefEvent extends Event
{
  type Key = AgentName
}

object AgentRefEvent
{
  final case class AgentAdded(uri: Uri) extends AgentRefEvent

  final case class AgentUpdated(uri: Uri) extends AgentRefEvent

  //case object AgentDeleted extends AgentRefEvent

  implicit val jsonCodec = TypedJsonCodec[AgentRefEvent](
    Subtype(deriveCodec[AgentAdded]),
    Subtype(deriveCodec[AgentUpdated]))
}
