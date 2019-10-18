package com.sos.jobscheduler.core.event.journal.test

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, JournalEvent, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
object TestJsonCodecs {

  implicit val TestKeyedEventJsonCodec = KeyedEventTypedJsonCodec[Event](
    KeyedSubtype[JournalEvent],
    KeyedSubtype[TestEvent])
}
