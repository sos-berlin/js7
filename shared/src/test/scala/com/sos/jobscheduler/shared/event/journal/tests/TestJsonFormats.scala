package com.sos.jobscheduler.shared.event.journal.tests

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
object TestJsonFormats {

  implicit val TestKeyedEventJsonFormat = KeyedEvent.typedJsonFormat[TestEvent](
    KeyedSubtype[TestEvent])
}
