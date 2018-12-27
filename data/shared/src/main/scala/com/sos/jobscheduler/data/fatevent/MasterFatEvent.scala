package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.NoKeyEvent
import com.sos.jobscheduler.data.master.MasterId

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterFatEvent extends FatEvent with NoKeyEvent

object MasterFatEvent
{
  final case class MasterReadyFat(masterId: MasterId, timezone: String) extends MasterFatEvent
  case object MasterReady

  implicit val jsonCodec: TypedJsonCodec[MasterFatEvent] = TypedJsonCodec(
    Subtype(deriveCodec[MasterReadyFat]))
}
