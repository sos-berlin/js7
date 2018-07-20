package com.sos.jobscheduler.master.data.events

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
sealed trait MasterEvent extends NoKeyEvent

object MasterEvent {
  final case class MasterReady(masterId: MasterId, timezone: ZoneId) extends MasterEvent
  case object MasterReady

  intelliJuseImport(zoneIdJsonDecoder)
  implicit val jsonCodec: TypedJsonCodec[MasterEvent] = TypedJsonCodec(
    Subtype(deriveCodec[MasterReady]))
}
