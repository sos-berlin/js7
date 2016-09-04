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
    assert((Snapshot(3)(eventId) map { _.toString }) == Snapshot("3")(eventId))
  }

  "Pattern matching ignores EventId" in {
    assertResult(3) {
      Snapshot(3)(EventId(100)) match {
        case Snapshot(o) ⇒ o
      }
    }
  }

  "JSON with object" in {
    case class A(number: Int)
    implicit val aJsonFormat = jsonFormat1(A)
    val o = Snapshot(A(111))(EventId(777))
    val json = """{
      "eventId": 777,
      "number": 111
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[Snapshot[A]])
  }

  "JSON with array" in {
    val o = Snapshot(List(111, 222))(EventId(777))
    val json = """{
      "eventId": 777,
      "value": [111, 222]
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[Snapshot[immutable.Seq[Int]]])
  }
}

