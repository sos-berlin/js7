package js7.data.event

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.test.OurTestSuite
import js7.data.event.KeyedEventTest.*
import js7.tester.CirceJsonTester.testJson
import org.scalactic.source

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTest extends OurTestSuite {

  "KeyedEvent apply 1" in {
    val e = 100 <-: StringEvent("HUNDRED")
    assert(e.key == 100)
    assert(e.event.string == "HUNDRED")
  }

  "KeyedEvent apply 2 without type parameter" in {
    val e = 100 <-: StringEvent("HUNDRED")
    assert(e.key == 100)
    assert(e.event.string == "HUNDRED")
  }

  "StringEvent" in {
    check(100 <-: StringEvent("HUNDRED"),
      json"""{
        "TYPE": "StringEvent",
        "Key": 100,
        "string": "HUNDRED"
      }""")
  }

  "IntEvent" in {
    check(100 <-: IntEvent(100),
      json"""{
        "TYPE": "IntEvent",
        "Key": 100,
        "int": 100
      }""")
  }

  "SimpleEvent" in {
    check(100 <-: SimpleEvent,
      json"""{
        "TYPE": "SimpleEvent",
        "Key": 100
      }""")
  }

  private def check(event: KeyedEvent[TestEvent], json: Json): Unit = {
    implicit val testEventJsonFormat: TypedJsonCodec[TestEvent] = TypedJsonCodec(
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

  private def checkSingletonKey(event: KeyedEvent[AEvent.type], json: Json)
    (using pos: source.Position)
  : Unit = {
    implicit val testEventJsonFormat: TypedJsonCodec[AEvent.type] =
      TypedJsonCodec[AEvent.type](
        Subtype.singleton(AEvent))
    testJson(event, json)
  }
}

private object KeyedEventTest {
  sealed trait TestEvent extends Event.IsKeyBase[TestEvent] {
    val keyCompanion: TestEvent.type = TestEvent
  }
  object TestEvent extends Event.CompanionForKey[Int, TestEvent] {
    implicit def implicitSelf: TestEvent.type = this
  }

  private case class StringEvent(string: String) extends TestEvent

  private case class IntEvent(int: Int) extends TestEvent

  private case object SimpleEvent extends TestEvent

  private object AEvent extends NoKeyEvent
}
