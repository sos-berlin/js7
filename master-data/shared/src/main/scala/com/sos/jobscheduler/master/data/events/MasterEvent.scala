package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.event.NoKeyEvent
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterEvent extends NoKeyEvent

object MasterEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class MasterReady(timezone: String, totalRunningTime: FiniteDuration)
  extends MasterEvent

  final case class MasterShutDown(clusterSwitchOver: Boolean) extends MasterEvent

  case object MasterTestEvent extends MasterEvent

  implicit val jsonCodec: TypedJsonCodec[MasterEvent] = TypedJsonCodec(
    Subtype(deriveCodec[MasterReady]),
    Subtype(deriveCodec[MasterShutDown]),
    Subtype(MasterTestEvent))
}
