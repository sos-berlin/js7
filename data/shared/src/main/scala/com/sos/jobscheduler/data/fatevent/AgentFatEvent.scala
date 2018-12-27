package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.agent.AgentPath

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentFatEvent extends FatEvent {
  type Key = AgentPath
}

object AgentFatEvent
{
  final case class AgentReadyFat(timezone: String) extends AgentFatEvent
  case object MasterReady

  implicit val jsonCodec: TypedJsonCodec[AgentFatEvent] = TypedJsonCodec(
    Subtype(deriveCodec[AgentReadyFat]))
}
