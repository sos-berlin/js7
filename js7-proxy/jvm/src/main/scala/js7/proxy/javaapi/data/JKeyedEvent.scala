package js7.proxy.javaapi.data

import js7.base.circeutils.CirceUtils._
import js7.data.event.{Event, KeyedEvent}
import js7.master.data.events.MasterKeyedEventJsonCodec

object JKeyedEvent
{
  def keyedEventToJson[E <: Event](keyedEvent: KeyedEvent[E]): String =
    MasterKeyedEventJsonCodec.encodeObject(keyedEvent).compactPrint
}
