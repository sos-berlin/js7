package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.KeyedEventTest._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.Json
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTest extends FreeSpec {

  "KeyedEvent apply 1" in {
    val e = KeyedEvent[StringEvent](100, StringEvent("HUNDRED"))
    assert(e.key == 100)
    assert(e.event.string == "HUNDRED")
  }

  "KeyedEvent apply 2 without type parameter" in {
    val e = KeyedEvent(StringEvent("HUNDRED"))(100)
    assert(e.key == 100)
    assert(e.event.string == "HUNDRED")
  }

  "StringEvent" in {
    check(KeyedEvent(StringEvent("HUNDRED"))(100),
      json"""{
        "TYPE": "StringEvent",
        "key": 100,
        "string": "HUNDRED"
      }""")
  }

  "IntEvent" in {
    check(KeyedEvent(IntEvent(100))(100),
      json"""{
        "TYPE": "IntEvent",
        "key": 100,
        "int": 100
      }""")
  }

  "SimpleEvent" in {
    check(KeyedEvent(SimpleEvent)(100),
      json"""{
        "TYPE": "SimpleEvent",
        "key": 100
      }""")
  }

  private def check(event: KeyedEvent[TestEvent], json: Json): Unit = {
    implicit val testEventJsonFormat = TypedJsonCodec[TestEvent](
      Subtype(deriveCodec[StringEvent]),
      Subtype(deriveCodec[IntEvent]),
      Subtype(SimpleEvent))
    testJson(event, json)
  }

  "SimpleEvent with NoKey" in {
    checkSingletonKey(KeyedEvent(AEvent),
      json"""{
        "TYPE": "AEvent"
      }""")
  }

  private def checkSingletonKey(event: KeyedEvent[AEvent.type], json: Json): Unit = {
    implicit val testEventJsonFormat = TypedJsonCodec[AEvent.type](
      Subtype(AEvent))
    testJson(event, json)
  }
}

private object KeyedEventTest {
  private sealed trait TestEvent extends Event {
    type Key = Int
  }

  private case class StringEvent(string: String) extends TestEvent

  private case class IntEvent(int: Int) extends TestEvent

  private case object SimpleEvent extends TestEvent

  private case object AEvent extends NoKeyEvent
}
