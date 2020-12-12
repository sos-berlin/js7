package js7.proxy.javaapi.data.event

import javax.annotation.Nonnull
import js7.base.circeutils.CirceUtils._
import js7.controller.data.ControllerState
import js7.data.event.{Event, KeyedEvent}

object JKeyedEvent
{
  @Nonnull
  def keyedEventToJson[E <: Event](@Nonnull keyedEvent: KeyedEvent[E]): String =
    ControllerState.keyedEventJsonCodec.encodeObject(keyedEvent).compactPrint
}
