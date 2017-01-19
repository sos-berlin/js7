package com.sos.scheduler.engine.data.event

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SnapshotTest extends FreeSpec {

  "map" in {
    val eventId = EventId(100)
    assert((Snapshot(eventId, 3) map { _.toString }) == Snapshot(eventId, "3"))
  }

  "Pattern matching ignores EventId" in {
    assertResult(3) {
      Snapshot(EventId(100), 3) match {
        case Snapshot(_, o) ⇒ o
      }
    }
  }

  "JSON with object" in {
    case class A(number: Int)
    implicit val aJsonFormat = jsonFormat1(A)
    val o = Snapshot(EventId(777), A(111))
    val json = """{
      "eventId": 777,
      "number": 111
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[Snapshot[A]])
  }

  "JSON with array" in {
    val o = Snapshot(EventId(777), List(111, 222))
    val json = """{
      "eventId": 777,
      "elements": [111, 222]
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[Snapshot[immutable.Seq[Int]]])
  }
}
