package js7.data.event

import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.event.EventSeqTest.*
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class EventSeqTest extends OurTestSuite:

  "JSON EventSeq.NonEmpty" in:
    checkTearableEventSeq(
      EventSeq.NonEmpty(List(
        Stamped(1, Timestamp.ofEpochMilli(123), "KEY" <-: MyEvent: KeyedEvent[TestEvent]))),
      json"""{
        "TYPE": "NonEmpty",
        "stamped": [
          {
            "eventId": 1,
            "timestamp": 123,
            "TYPE": "MyEvent",
            "Key": "KEY"
            }
          ]
        }""")

  "JSON EventSeq.Empty" in:
    checkTearableEventSeq[TestEvent](
      EventSeq.Empty(EventId(123)),
      json"""{
        "TYPE": "Empty",
        "lastEventId": 123
        }""")

  "JSON TearableEventSeq.Torn" in:
    checkTearableEventSeq[TestEvent](TearableEventSeq.Torn(7),
      json"""{
        "TYPE": "Torn",
        "after": 7
      }""")

  private def checkTearableEventSeq[E: Encoder.AsObject: Decoder](
    eventSeq: TearableEventSeq[Seq, E],
    json: Json)
  =
    testJson(eventSeq, json)
    eventSeq match
      case eventSeq: EventSeq[Seq, E] => testJson(eventSeq, json)
      case _ =>


object EventSeqTest:
  private trait TestEvent extends Event.IsKeyBase[TestEvent]:
    val keyCompanion: TestEvent.type = TestEvent

  private object TestEvent extends Event.CompanionForKey[String, TestEvent]:
    implicit def implicitSelf: TestEvent.type = this

    implicit val jsonCodec: TypedJsonCodec[TestEvent] = TypedJsonCodec(
      Subtype.singleton(MyEvent))

  private case object MyEvent extends TestEvent
