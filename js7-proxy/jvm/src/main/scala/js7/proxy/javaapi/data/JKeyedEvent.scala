package js7.proxy.javaapi.data

import js7.base.circeutils.CirceUtils._
import js7.controller.data.events.ControllerKeyedEventJsonCodec
import js7.data.event.{Event, KeyedEvent}

object JKeyedEvent
{
  def keyedEventToJson[E <: Event](keyedEvent: KeyedEvent[E]): String =
    ControllerKeyedEventJsonCodec.encodeObject(keyedEvent).compactPrint
}
