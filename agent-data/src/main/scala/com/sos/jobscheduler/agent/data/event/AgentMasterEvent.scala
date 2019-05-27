package com.sos.jobscheduler.agent.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.event.NoKeyEvent
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait AgentMasterEvent extends NoKeyEvent

object AgentMasterEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class AgentReadyForMaster(timezone: String, totalRunningTime: FiniteDuration) extends AgentMasterEvent

  implicit val jsonCodec = TypedJsonCodec[AgentMasterEvent](
    Subtype(deriveCodec[AgentReadyForMaster]))
}
