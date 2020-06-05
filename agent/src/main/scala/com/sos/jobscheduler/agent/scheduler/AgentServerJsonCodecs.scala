package js7.agent.scheduler

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.JournalState

private[scheduler] object AgentServerJsonCodecs
{
  val jsonCodec = TypedJsonCodec[Any](
    Subtype[JournalState],
    Subtype[RegisteredMaster])
}
