package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.{UnknownClassForJsonException, UnknownJsonTypeException}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodecTest._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTypedJsonCodecTest extends FreeSpec {

  "encode and decode" in {
    testJson[KeyedEvent[TestEvent]](NoKey <-: E0,      json"""{ "TYPE": "E0" }""")
    testJson[KeyedEvent[TestEvent]]("A"   <-: E1(7),   json"""{ "TYPE": "E1", "key": "A", "int": 7 }""")
    testJson[KeyedEvent[TestEvent]]("B"   <-: E2("X"), json"""{ "TYPE": "E2", "key": "B", "string": "X" }""")
    testJson[KeyedEvent[TestEvent]](1     <-: E3(2),   json"""{ "TYPE": "E3", "key": 1, "int": 2 }""")
  }

  "encode unknown subclass" in {
    intercept[UnknownClassForJsonException] {
      (KeyedEvent(NotRegistered(1)): KeyedEvent[TestEvent]).asJson
    }.getMessage should include (
      "Class com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodecTest$NotRegistered is not registered with TypedJsonCodec[com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodecTest$TestEvent]")
  }

  "decode unknown subclass" in {
    intercept[UnknownJsonTypeException] {
      """{ "TYPE": "UNKNOWN" }""".parseJson.as[KeyedEvent[TestEvent]].force
    }.getMessage should include ("""Unexpected JSON {"TYPE": "UNKNOWN"} for class 'TestEvent'""")
  }

  "typenameToClassOption" in {
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("UNKNOWN") == None)
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("TestEvent") == Some(classOf[TestEvent]))
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("E0") == Some(E0.getClass))
    assert(TestEventKeyedEventTypedJsonCodec.typenameToClassOption("E1") == Some(classOf[E1]))
  }
}

object KeyedEventTypedJsonCodecTest {
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

  private implicit val StringEventJsonCodec: TypedJsonCodec[StringEvent] = TypedJsonCodec[StringEvent](
    Subtype[E1],
    Subtype[E2])

  private implicit val IntEventJsonCodec: TypedJsonCodec[IntEvent] = TypedJsonCodec[IntEvent](
    Subtype[E3])

  private implicit val TestEventKeyedEventTypedJsonCodec: KeyedEventTypedJsonCodec[TestEvent] = KeyedEventTypedJsonCodec[TestEvent](
    KeyedSubtype(E0),
    KeyedSubtype[StringEvent],
    KeyedSubtype[IntEvent])
}
