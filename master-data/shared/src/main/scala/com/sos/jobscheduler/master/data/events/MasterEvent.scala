package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.NoKeyEvent

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterEvent extends NoKeyEvent

object MasterEvent {
  sealed trait MasterReady extends MasterEvent
  case object MasterReady extends MasterReady

  implicit val jsonCodec: TypedJsonCodec[MasterEvent] = TypedJsonCodec(
    Subtype(MasterReady))
}
