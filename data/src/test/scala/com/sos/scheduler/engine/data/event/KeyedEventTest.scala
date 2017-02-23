package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.KeyedEventTest._
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json._

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
      """{
        "TYPE": "StringEvent",
        "key": 100,
        "string": "HUNDRED"
      }""")
  }

  "IntEvent" in {
    check(KeyedEvent(IntEvent(100))(100),
      """{
        "TYPE": "IntEvent",
        "key": 100,
        "int": 100
      }""")
  }

  "SimpleEvent" in {
    check(KeyedEvent(SimpleEvent)(100),
      """{
        "TYPE": "SimpleEvent",
        "key": 100
      }""")
  }

  private def check(event: KeyedEvent[TestEvent], json: String): Unit = {
    implicit val testEventJsonFormat = TypedJsonFormat[TestEvent](
      Subtype(jsonFormat1(StringEvent)),
      Subtype(jsonFormat1(IntEvent)),
      Subtype(jsonFormat0(() ⇒ SimpleEvent)))
    val jsValue = json.parseJson
    assert (event.toJson == jsValue)
    assert (event == jsValue.convertTo[KeyedEvent[TestEvent]] )
  }

  "SimpleEvent with NoKey" in {
    checkSingletonKey(KeyedEvent(AEvent),
      """{
        "TYPE": "AEvent"
      }""")
  }

  private def checkSingletonKey(event: KeyedEvent[AEvent.type], json: String): Unit = {
    implicit val testEventJsonFormat = TypedJsonFormat[AEvent.type](
      Subtype(jsonFormat0(() ⇒ AEvent)))
    val jsValue = json.parseJson
    assert (event.toJson == jsValue)
    assert (event == jsValue.convertTo[KeyedEvent[AEvent.type]] )
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
