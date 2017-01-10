package com.sos.scheduler.engine.data.event.custom

import com.sos.scheduler.engine.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, KeyedEvent}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class VariablesCustomEventTest extends FreeSpec {

  private implicit val anyKeyedEventJsonFormat = KeyedEvent.typedJsonFormat[Event](
    KeyedSubtype[CustomEvent])

  "JSON" in {
    val jsValue = """{
        "key": "KEY",
        "TYPE": "VariablesCustomEvent",
        "variables": {
          "KEY": "VALUE",
          "a": "123"
        }
       }""".parseJson
    val event = VariablesCustomEvent(Map(
      "KEY" → "VALUE",
      "a" → "123"))
    val keyedEvent = KeyedEvent(event)(CustomEvent.Key("KEY")): AnyKeyedEvent
    assert(keyedEvent.toJson == jsValue)
    assert(jsValue.convertTo[AnyKeyedEvent] == keyedEvent)
  }
}
