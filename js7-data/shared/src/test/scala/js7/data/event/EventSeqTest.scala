package js7.data.event

import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventSeqTest extends AnyFreeSpec {

  private object TestEvent extends Event {
    type Key = String
  }
  private implicit val jsonCodec: TypedJsonCodec[TestEvent.type] = TypedJsonCodec(
    Subtype(TestEvent))

  "JSON EventSeq.NonEmpty" in {
    checkTearableEventSeq(
      EventSeq.NonEmpty(List(
        Stamped(1, Timestamp.ofEpochMilli(123), KeyedEvent(TestEvent)("KEY")))),
      json"""{
        "TYPE": "NonEmpty",
        "stamped": [
          {
            "eventId": 1,
            "timestamp": 123,
            "TYPE": "TestEvent",
            "Key": "KEY"
            }
          ]
        }""")
  }

  "JSON EventSeq.Empty" in {
    checkTearableEventSeq[TestEvent.type](
      EventSeq.Empty(EventId(123)),
      json"""{
        "TYPE": "Empty",
        "lastEventId": 123
        }""")
  }

  "JSON TearableEventSeq.Torn" in {
    checkTearableEventSeq[TestEvent.type](TearableEventSeq.Torn(7),
      json"""{
        "TYPE": "Torn",
        "after": 7
      }""")
  }

  private def checkTearableEventSeq[E: Encoder.AsObject: Decoder](
    eventSeq: TearableEventSeq[Seq, E],
    json: Json)
  = {
    testJson(eventSeq, json)
    eventSeq match {
      case eventSeq: EventSeq[Seq, E] => testJson(eventSeq, json)
      case _ =>
    }
  }
}
