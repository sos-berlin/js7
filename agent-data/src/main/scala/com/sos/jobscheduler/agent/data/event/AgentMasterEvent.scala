package com.sos.jobscheduler.agent.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.event.NoKeyEvent
import java.time.ZoneId

/**
  * @author Joacim Zschimmer
  */
trait AgentMasterEvent extends NoKeyEvent

object AgentMasterEvent
{
  intelliJuseImport(zoneIdJsonEncoder)

  final case class AgentReadyForMaster(timezone: ZoneId) extends AgentMasterEvent

  implicit val jsonCodec = TypedJsonCodec[AgentMasterEvent](
    Subtype(deriveCodec[AgentReadyForMaster]))
}
