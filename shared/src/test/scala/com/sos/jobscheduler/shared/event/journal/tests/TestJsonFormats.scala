package com.sos.jobscheduler.shared.event.journal.tests

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
object TestJsonFormats {

  implicit val TestKeyedEventJsonFormat = KeyedEvent.typedJsonFormat[Event](
    KeyedSubtype[TestEvent])
}
