package com.sos.scheduler.engine.shared.event.journal.tests

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.scheduler.engine.data.event.{Event, KeyedEvent}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
object TestJsonFormats {

  implicit val TestKeyedEventJsonFormat = KeyedEvent.typedJsonFormat[Event](
    KeyedSubtype[TestEvent])
}
