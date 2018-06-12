package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.{Decoder, ObjectEncoder}
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.language.higherKinds

/**
  * @author Joacim Zschimmer
  */
final class EventSeqTest extends FreeSpec {

  private object TestEvent extends Event {
    type Key = String
  }
  private implicit val jsonCodec = TypedJsonCodec[TestEvent.type](
    Subtype(TestEvent))

  "JSON EventSeq.NonEmpty" in {
    checkTearableEventSeq(
      EventSeq.NonEmpty(List(
        Stamped(1, Timestamp.ofEpochMilli(123), KeyedEvent(TestEvent)("KEY")))),
      """{
        "TYPE": "NonEmpty",
        "stamped": [
          {
            "eventId": 1,
            "timestamp": 123,
            "TYPE": "TestEvent",
            "key": "KEY"
            }
          ]
        }""")
  }

  "JSON EventSeq.Empty" in {
    checkTearableEventSeq[TestEvent.type](
      EventSeq.Empty(EventId(123)),
      """{
        "TYPE": "Empty",
        "lastEventId": 123
        }""")
  }

  "JSON TearableEventSeq.Torn" in {
    checkTearableEventSeq[TestEvent.type](TearableEventSeq.Torn(7),
      """{
        "TYPE": "Torn",
        "oldestKnownEventId": 7
      }""")
  }

  private def checkTearableEventSeq[E: ObjectEncoder: Decoder](eventSeq: TearableEventSeq[Seq, E], json: String) = {
    testJson(eventSeq, json)
    eventSeq match {
      case eventSeq: EventSeq[Seq, E] ⇒ testJson(eventSeq, json)
      case _ ⇒
    }
  }
}
