package com.sos.jobscheduler.master.gui.data.event

import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.language.higherKinds

final class EventSeqTest extends FreeSpec {

  private object TestEvent extends Event {
    type Key = String
  }

  "JSON EventSeq.NonEmpty" in {
    checkTearableEventSeq(
      EventSeq.NonEmpty(List(
        Stamped(1, KeyedEvent(TestEvent)("KEY")))),
      """{
        "TYPE": "NonEmpty",
        "eventSnapshots": [
          {
            "TYPE": "TestEvent",
            "key": "KEY",
            "eventId": 1
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

  "JSON EventSeq.Torn" in {
    checkTearableEventSeq[TestEvent.type](EventSeq.Torn,
      """{ "TYPE": "Torn" }""")
  }

  private def checkTearableEventSeq[E: Decoder](eventSeq: TearableEventSeq[Seq, E], json: String) = {
    //assert(eventSeq.asJson == parse(json).toTry.get)
    assert(parse(json).toTry.get.as[TearableEventSeq[Seq, E]].toTry.get == eventSeq)
    eventSeq match {
      case eventSeq: EventSeq[Seq, E] ⇒ checkEventSeq(eventSeq, json)
      case _ ⇒
    }
  }

  private def checkEventSeq[E: Decoder](eventSeq: EventSeq[Seq, E], json: String) = {
    //assert(eventSeq.asJson == parse(json).toTry.get)
    assert(parse(json).toTry.get.as[EventSeq[Seq, E]].toTry.get == eventSeq)
  }
}
