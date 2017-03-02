package com.sos.jobscheduler.data.event

import org.scalatest.FreeSpec
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class StampedTest extends FreeSpec {

  "map" in {
    val eventId = EventId(100)
    assert((Stamped(eventId, 3) map { _.toString }) == Stamped(eventId, "3"))
  }

  "Pattern matching ignores EventId" in {
    assertResult(3) {
      Stamped(EventId(100), 3) match {
        case Stamped(_, o) ⇒ o
      }
    }
  }

  "JSON with object" in {
    case class A(number: Int)
    implicit val aJsonFormat = jsonFormat1(A)
    val o = Stamped(EventId(777), A(111))
    val json = """{
      "eventId": 777,
      "number": 111
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[Stamped[A]])
  }

  "JSON with array" in {
    val o = Stamped(EventId(777), List(111, 222))
    val json = """{
      "eventId": 777,
      "elements": [111, 222]
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[Stamped[immutable.Seq[Int]]])
  }
}
