package js7.data.event

import io.circe.DecodingFailure
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.TypedJsonCodec.UnknownClassForJsonException
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.KeyedEventTypedJsonCodecTest._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTypedJsonCodecTest extends AnyFreeSpec
{
  "encode and decode" in {
    testJson[KeyedEvent[TestEvent]](NoKey <-: E0,      json"""{ "TYPE": "E0" }""")
    testJson[KeyedEvent[TestEvent]]("A"   <-: E1(7),   json"""{ "TYPE": "E1", "Key": "A", "int": 7 }""")
    testJson[KeyedEvent[TestEvent]]("B"   <-: E2("X"), json"""{ "TYPE": "E2", "Key": "B", "string": "X" }""")
    testJson[KeyedEvent[TestEvent]](1     <-: E3(2),   json"""{ "TYPE": "E3", "Key": 1, "int": 2 }""")
  }

  "encode unknown subclass" in {
    intercept[UnknownClassForJsonException] {
      (KeyedEvent(NotRegistered(1)): KeyedEvent[TestEvent]).asJson
    }.getMessage should include (
      "Class 'KeyedEventTypedJsonCodecTest.NotRegistered' is not registered in " +
        "js7.data.event.KeyedEventTypedJsonCodecTest.TestEventKeyedEventTypedJsonCodec: " +
        "KeyedEventTypedJsonCodec[KeyedEventTypedJsonCodecTest.TestEvent]")
  }

  "decode unknown subclass" in {
    assert("""{ "TYPE": "UNKNOWN" }""".parseJsonOrThrow.as[KeyedEvent[TestEvent]]
      == Left(DecodingFailure(
      """Unexpected JSON {"TYPE": "UNKNOWN", ...} for """ +
        "js7.data.event.KeyedEventTypedJsonCodecTest.TestEventKeyedEventTypedJsonCodec: " +
        "KeyedEventTypedJsonCodec[KeyedEventTypedJsonCodecTest.TestEvent]",
      Nil)))
  }

  "Union" in {
    implicit val ab: KeyedEventTypedJsonCodec[Event] =
      KeyedEventTypedJsonCodec[E0.type](KeyedSubtype(E0)) |
      KeyedEventTypedJsonCodec[StringEvent](KeyedSubtype[StringEvent]) |
      KeyedEventTypedJsonCodec[IntEvent](KeyedSubtype[IntEvent])

    testJson[KeyedEvent[Event]](NoKey <-: E0,      json"""{ "TYPE": "E0" }""")
    testJson[KeyedEvent[Event]]("A"   <-: E1(7),   json"""{ "TYPE": "E1", "Key": "A", "int": 7 }""")
    testJson[KeyedEvent[Event]]("B"   <-: E2("X"), json"""{ "TYPE": "E2", "Key": "B", "string": "X" }""")
    testJson[KeyedEvent[Event]](1     <-: E3(2),   json"""{ "TYPE": "E3", "Key": 1, "int": 2 }""")
  }

  "Unknown TYPE" in {
    implicit val e0KeyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[E0.type] =
      KeyedEventTypedJsonCodec(
        KeyedSubtype(E0))

    testJson[KeyedEvent[E0.type]](NoKey <-: E0, json"""{ "TYPE": "E0" }""")
    assert(json"""{ "TYPE": "E0" }""".as[KeyedEvent[E0.type]].isRight)
    assert(json"""{ "TYPE": "UNKNOWN" }""".as[KeyedEvent[E0.type]] == Left(DecodingFailure(
      """Unexpected JSON {"TYPE": "UNKNOWN", ...} for """ +
        "js7.data.event.KeyedEventTypedJsonCodecTest#e0KeyedEventTypedJsonCodec: " +
        "KeyedEventTypedJsonCodec[KeyedEventTypedJsonCodecTest.E0]",
      Nil)))
  }

  "typenameToClassOption" in {
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("UNKNOWN") == None)
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("TestEvent") == Some(classOf[TestEvent]))
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("E0") == Some(E0.getClass))
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("E1") == Some(classOf[E1]))
  }

  "isOfType" in {
    val a1Json = json"""{ "TYPE": "E1", "Key": "A", "int": 7 }"""
    val xJson = json"""{ "TYPE":  "X"}"""
    assert(StringEventJsonCodec.isOfType[E1](a1Json))
    assert(!StringEventJsonCodec.isOfType[E1](xJson))
  }
}

object KeyedEventTypedJsonCodecTest
{
  sealed trait TestEvent extends Event

  case object E0 extends TestEvent with NoKeyEvent

  sealed trait StringEvent extends TestEvent {
    type Key = String
  }
  sealed trait IntEvent extends TestEvent  {
    type Key = Int
  }

  @JsonCodec
  final case class E1(int: Int) extends StringEvent

  @JsonCodec
  final case class E2(string: String) extends StringEvent

  @JsonCodec
  final case class NotRegistered(int: Int) extends TestEvent with NoKeyEvent

  @JsonCodec
  final case class E3(int: Int) extends IntEvent

  private implicit val StringEventJsonCodec: TypedJsonCodec[StringEvent] =
    TypedJsonCodec(
      Subtype[E1],
      Subtype[E2])

  private implicit val IntEventJsonCodec: TypedJsonCodec[IntEvent] =
    TypedJsonCodec(
      Subtype[E3])

  private implicit val TestEventKeyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[TestEvent] =
    KeyedEventTypedJsonCodec(
      KeyedSubtype(E0),
      KeyedSubtype[StringEvent],
      KeyedSubtype[IntEvent])
}
