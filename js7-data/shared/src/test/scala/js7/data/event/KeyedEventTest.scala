package js7.data.event

import io.circe.Json
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.KeyedEventTest._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTest extends AnyFreeSpec {

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
        "Key": 100,
        "string": "HUNDRED"
      }""")
  }

  "IntEvent" in {
    check(KeyedEvent(IntEvent(100))(100),
      json"""{
        "TYPE": "IntEvent",
        "Key": 100,
        "int": 100
      }""")
  }

  "SimpleEvent" in {
    check(KeyedEvent(SimpleEvent)(100),
      json"""{
        "TYPE": "SimpleEvent",
        "Key": 100
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
