package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
trait FatEvent extends Event

object FatEvent
{
  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[FatEvent] =
    KeyedEventTypedJsonCodec[FatEvent](
      KeyedSubtype[MasterFatEvent],
      KeyedSubtype[OrderFatEvent])
}
