package js7.core.event.journal.test

import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
object TestJsonCodecs {

  implicit val TestKeyedEventJsonCodec = KeyedEventTypedJsonCodec[Event](
    KeyedSubtype[JournalEvent],
    KeyedSubtype[TestEvent])
}
