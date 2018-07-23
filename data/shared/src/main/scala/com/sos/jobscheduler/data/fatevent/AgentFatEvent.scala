package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.agent.AgentPath
import java.time.ZoneId

/**
  * @author Joacim Zschimmer
  */
sealed trait AgentFatEvent extends FatEvent {
  type Key = AgentPath
}

object AgentFatEvent
{
  intelliJuseImport(zoneIdJsonDecoder)

  final case class AgentReadyFat(timezone: ZoneId) extends AgentFatEvent
  case object MasterReady

  implicit val jsonCodec: TypedJsonCodec[AgentFatEvent] = TypedJsonCodec(
    Subtype(deriveCodec[AgentReadyFat]))
}
