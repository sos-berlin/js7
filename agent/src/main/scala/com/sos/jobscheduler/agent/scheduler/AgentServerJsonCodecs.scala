package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.JournalState

private[scheduler] object AgentServerJsonCodecs
{
  val jsonCodec = TypedJsonCodec[Any](
    Subtype[JournalState],
    Subtype[RegisteredMaster])
}
