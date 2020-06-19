package js7.data.fatevent

import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
trait FatEvent extends Event

object FatEvent
{
  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[FatEvent] =
    KeyedEventTypedJsonCodec[FatEvent](
      KeyedSubtype[ControllerFatEvent],
      KeyedSubtype[AgentFatEvent],
      KeyedSubtype[OrderFatEvent])
}
