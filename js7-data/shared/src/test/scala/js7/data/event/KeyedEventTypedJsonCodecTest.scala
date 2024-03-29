package js7.data.event

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, DecodingFailure}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.TypedJsonCodec.UnknownClassForJsonException
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.test.OurTestSuite
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.KeyedEventTypedJsonCodecTest.*
import js7.tester.CirceJsonTester.testJson
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTypedJsonCodecTest extends OurTestSuite:
  "encode and decode" in:
    testJson[KeyedEvent[TestEvent]](NoKey <-: E0,      json"""{ "TYPE": "E0" }""")
    testJson[KeyedEvent[TestEvent]]("A"   <-: E1(7),   json"""{ "TYPE": "E1", "Key": "A", "int": 7 }""")
    testJson[KeyedEvent[TestEvent]]("B"   <-: E2("X"), json"""{ "TYPE": "E2", "Key": "B", "string": "X" }""")
    testJson[KeyedEvent[TestEvent]](1     <-: E3(2),   json"""{ "TYPE": "E3", "Key": 1, "int": 2 }""")

  "encode unknown subclass" in:
    intercept[UnknownClassForJsonException] {
      (KeyedEvent(NotRegistered(1)): KeyedEvent[TestEvent]).asJson
    }.getMessage should include (
      "Class 'KeyedEventTypedJsonCodecTest.NotRegistered' is not registered in " +
        "js7.data.event.KeyedEventTypedJsonCodecTest.TestEvent.jsonCodec: " +
        "KeyedEventTypedJsonCodec[KeyedEventTypedJsonCodecTest.TestEvent]")

  "decode unknown subclass" in:
    assert("""{ "TYPE": "UNKNOWN" }""".parseJsonOrThrow.as[KeyedEvent[TestEvent]]
      == Left(DecodingFailure(
      """Unexpected JSON {"TYPE": "UNKNOWN", ...} for """ +
      """js7.data.event.KeyedEventTypedJsonCodecTest.TestEvent.jsonCodec: """ +
      """KeyedEventTypedJsonCodec[KeyedEventTypedJsonCodecTest.TestEvent]"""
,
      Nil)))

  "Union" in:
    implicit val ab: KeyedEventTypedJsonCodec[Event] =
      KeyedEventTypedJsonCodec[E0.type](KeyedSubtype.singleton(using NoKeyEvent)(E0)) |
      KeyedEventTypedJsonCodec[StringEvent](KeyedSubtype[StringEvent]) |
      KeyedEventTypedJsonCodec[IntEvent](KeyedSubtype[IntEvent])

    testJson[KeyedEvent[Event]](NoKey <-: E0,      json"""{ "TYPE": "E0" }""")
    testJson[KeyedEvent[Event]]("A"   <-: E1(7),   json"""{ "TYPE": "E1", "Key": "A", "int": 7 }""")
    testJson[KeyedEvent[Event]]("B"   <-: E2("X"), json"""{ "TYPE": "E2", "Key": "B", "string": "X" }""")
    testJson[KeyedEvent[Event]](1     <-: E3(2),   json"""{ "TYPE": "E3", "Key": 1, "int": 2 }""")

  "Unknown TYPE" in:
    implicit val e0KeyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[E0.type] =
      KeyedEventTypedJsonCodec(
        KeyedSubtype.singleton(using NoKeyEvent)(E0))

    testJson[KeyedEvent[E0.type]](NoKey <-: E0, json"""{ "TYPE": "E0" }""")
    assert(json"""{ "TYPE": "E0" }""".as[KeyedEvent[E0.type]].isRight)
    assert(json"""{ "TYPE": "UNKNOWN" }""".as[KeyedEvent[E0.type]] == Left(DecodingFailure(
      """Unexpected JSON {"TYPE": "UNKNOWN", ...} for """ +
        "js7.data.event.KeyedEventTypedJsonCodecTest#e0KeyedEventTypedJsonCodec: " +
        "KeyedEventTypedJsonCodec[KeyedEventTypedJsonCodecTest.E0]",
      Nil)))

  "typenameToClassOption" in:
    assert(TestEvent.jsonCodec.typenameToClassOption("UNKNOWN") == None)
    assert(TestEvent.jsonCodec.typenameToClassOption("TestEvent") == Some(classOf[TestEvent]))
    assert(TestEvent.jsonCodec.typenameToClassOption("E0") == Some(E0.getClass))
    assert(TestEvent.jsonCodec.typenameToClassOption("E1") == Some(classOf[E1]))

  "isOfType" in:
    val a1Json = json"""{ "TYPE": "E1", "Key": "A", "int": 7 }"""
    val xJson = json"""{ "TYPE":  "X"}"""
    assert(StringEvent.jsonCodec.isOfType[E1](a1Json))
    assert(!StringEvent.jsonCodec.isOfType[E1](xJson))


object KeyedEventTypedJsonCodecTest:
  sealed trait TestEvent extends Event
  object TestEvent:
    implicit val jsonCodec: KeyedEventTypedJsonCodec[TestEvent] =
      KeyedEventTypedJsonCodec(
        KeyedSubtype.singleton(using NoKeyEvent)(E0),
        KeyedSubtype[StringEvent],
        KeyedSubtype[IntEvent])

  case object E0 extends TestEvent, NoKeyEvent

  sealed trait StringEvent extends TestEvent, Event.IsKeyBase[StringEvent]:
    val keyCompanion: StringEvent.type = StringEvent
  object StringEvent extends Event.CompanionForKey[String, StringEvent]:
    override implicit def implicitSelf: StringEvent.type = this
    implicit val jsonCodec: TypedJsonCodec[StringEvent] =
      TypedJsonCodec(
        Subtype[E1],
        Subtype[E2])

  sealed trait IntEvent extends TestEvent, Event.IsKeyBase[IntEvent]:
    override val keyCompanion: IntEvent.type = IntEvent
  object IntEvent extends Event.CompanionForKey[Int, IntEvent]:
    override implicit def implicitSelf: IntEvent.type = this

  final case class E1(int: Int) extends StringEvent
  object E1:
    implicit val jsonCodec: Codec.AsObject[E1] = deriveCodec

  final case class E2(string: String) extends StringEvent
  object E2:
    implicit val jsonCodec: Codec.AsObject[E2] = deriveCodec

  final case class NotRegistered(int: Int) extends TestEvent, NoKeyEvent
  object NotRegistered:
    implicit val jsonCodec: Codec.AsObject[NotRegistered] = deriveCodec

  final case class E3(int: Int) extends IntEvent
  object E3:
    implicit val jsonCodec: Codec.AsObject[E3] = deriveCodec

  private implicit val IntEventJsonCodec: TypedJsonCodec[IntEvent] =
    TypedJsonCodec(
      Subtype[E3])
