package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.event.NoKeyEvent
import com.sos.jobscheduler.data.master.MasterId
import java.time.ZoneId

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterFatEvent extends FatEvent with NoKeyEvent

object MasterFatEvent
{
  final case class MasterReadyFat(masterId: MasterId, timezone: ZoneId) extends MasterFatEvent
  case object MasterReady

  intelliJuseImport(zoneIdJsonDecoder)

  implicit val jsonCodec: TypedJsonCodec[MasterFatEvent] = TypedJsonCodec(
    Subtype(deriveCodec[MasterReadyFat]))
}
