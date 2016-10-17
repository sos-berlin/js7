package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.Seq
import scala.language.higherKinds
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class EventSeqTest extends FreeSpec {

  private object TestEvent extends Event {
    type Key = String
    implicit val jsonFormat = TypedJsonFormat[TestEvent.type](
      Subtype(jsonFormat0(() ⇒ TestEvent)))
  }

  "JSON EventSeq.NonEmpty" in {
    check(
      EventSeq.NonEmpty(List(
        Snapshot(1, KeyedEvent(TestEvent)("KEY")))),
      """{
        "events": [
          {
            "TYPE": "TestEvent",
            "key": "KEY",
            "eventId": 1
            }
          ]
        }""")
  }

  "JSON EventSeq.Empty" in {
    check[TestEvent.type](
      EventSeq.Empty(EventId(123)),
      """{
        "lastEventId": 123
        }""")
  }

  "JSON EventSeq.Teared" in {
    check[TestEvent.type](EventSeq.Teared, "{}")
  }

  private def check[E: RootJsonFormat](eventSeq: EventSeq[Seq, E], json: String) = {
    assert(eventSeq.toJson == json.parseJson)
    assert(json.parseJson.convertTo[EventSeq[Seq, E]] == eventSeq)
  }
}
