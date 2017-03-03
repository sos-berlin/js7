package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.language.higherKinds
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class EventSeqTest extends FreeSpec {

  private object TestEvent extends Event {
    type Key = String
    implicit val jsonFormat = TypedJsonFormat[TestEvent.type](
      Subtype(jsonFormat0(() ⇒ TestEvent)))
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
      """{
        "TYPE": "Torn"
      }""")
  }

  private def checkTearableEventSeq[E: RootJsonFormat](eventSeq: TearableEventSeq[Seq, E], json: String) = {
    assert(eventSeq.toJson == json.parseJson)
    assert(json.parseJson.convertTo[TearableEventSeq[Seq, E]] == eventSeq)
    eventSeq match {
      case eventSeq: EventSeq[Seq, E] ⇒ checkEventSeq(eventSeq, json)
      case _ ⇒
    }
  }

  private def checkEventSeq[E: RootJsonFormat](eventSeq: EventSeq[Seq, E], json: String) = {
    assert(eventSeq.toJson == json.parseJson)
    assert(json.parseJson.convertTo[EventSeq[Seq, E]] == eventSeq)
  }
}
