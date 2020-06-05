package js7.data.fatevent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentRefPath

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentFatEvent extends FatEvent {
  type Key = AgentRefPath
}

object AgentFatEvent
{
  final case class AgentReadyFat(timezone: String) extends AgentFatEvent

  implicit val jsonCodec: TypedJsonCodec[AgentFatEvent] = TypedJsonCodec(
    Subtype(deriveCodec[AgentReadyFat]))
}
