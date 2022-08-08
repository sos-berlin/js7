package js7.data_for_java.event

import javax.annotation.Nonnull
import js7.base.circeutils.CirceUtils.RichJsonObject
import js7.data.controller.ControllerState
import js7.data.event.{Event, KeyedEvent}

object JKeyedEvent
{
  @Nonnull
  def keyedEventToJson[E <: Event](@Nonnull keyedEvent: KeyedEvent[E]): String =
    ControllerState.keyedEventJsonCodec.encodeObject(keyedEvent).compactPrint
}
