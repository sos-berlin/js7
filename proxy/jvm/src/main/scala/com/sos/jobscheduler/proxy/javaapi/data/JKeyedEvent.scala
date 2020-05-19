package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.master.data.events.MasterKeyedEventJsonCodec

object JKeyedEvent
{
  def keyedEventToJson[E <: Event](keyedEvent: KeyedEvent[E]): String =
    MasterKeyedEventJsonCodec.encodeObject(keyedEvent).compactPrint
}
