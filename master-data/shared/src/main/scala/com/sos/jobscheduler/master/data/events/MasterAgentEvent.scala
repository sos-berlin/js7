package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Event
import java.time.ZoneId

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterAgentEvent extends Event {
  type Key = AgentPath
}

object MasterAgentEvent
{
  intelliJuseImport(zoneIdJsonEncoder)

  final case class AgentReady(timezone: ZoneId) extends MasterAgentEvent

  implicit val jsonCodec = TypedJsonCodec[MasterAgentEvent](
    Subtype(deriveCodec[AgentReady])
  )
}
