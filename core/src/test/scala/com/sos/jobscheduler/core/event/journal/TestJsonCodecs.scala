package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype

/**
  * @author Joacim Zschimmer
  */
object TestJsonCodecs {

  implicit val TestKeyedEventJsonCodec = KeyedEventTypedJsonCodec[TestEvent](
    KeyedSubtype[TestEvent])
}
