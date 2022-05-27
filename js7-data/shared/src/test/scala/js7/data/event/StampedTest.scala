package js7.data.event

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StampedTest extends AnyFreeSpec {

  "map" in {
    val eventId = EventId(100)
    assert(Stamped(eventId, Timestamp.ofEpochMilli(123), 3).map(_.toString) ==
      Stamped(eventId, Timestamp.ofEpochMilli(123), "3"))
  }

  "JSON with object" in {
    case class A(number: Int)
    implicit val codec = deriveCodec[A]
    testJson(Stamped(EventId(777), Timestamp.ofEpochMilli(123), A(111)),
      json"""{
        "eventId": 777,
        "timestamp": 123,
        "number": 111
      }""")
    testJson(Stamped(EventId(123007), Timestamp.ofEpochMilli(123), A(111)),
      json"""{
        "eventId": 123007,
        "number": 111
      }""")
  }

  "JSON with array" in {
    testJson(Stamped(EventId(777), Timestamp.ofEpochMilli(123), List(111, 222)),
      json"""{
        "eventId": 777,
        "timestamp": 123,
        "array": [111, 222]
      }""")
    testJson(Stamped(EventId(123007), Timestamp.ofEpochMilli(123), List(111, 222)),
      json"""{
        "eventId": 123007,
        "array": [111, 222]
      }""")
  }

  "Stamped[simple type] is not supported" in {
    // Rejected because a field "value" may duplicate an object field "value".
    // Stamped is used only for objects and arrays, not for simple values.
    intercept[RuntimeException] {
      Stamped(EventId(777), Timestamp.ofEpochMilli(123), "VALUE").asJson.as[Stamped[String]]
    }
    //testJson(Stamped(EventId(777), Timestamp.ofEpochMilli(123), "VALUE"),
    //  json"""{
    //    "eventId": 777,
    //    "timestamp": 123,
    //    "value": "VALUE"
    //  }""")
  }

  "checkOrdering" in {
    assert(Stamped.checkOrdering(0, Nil) == Right(()))
    assert(Stamped.checkOrdering(7, Nil) == Right(()))
    assert(Stamped.checkOrdering(0, Seq(Stamped(1, ""), Stamped(2, "")))
      == Right(()))
    assert(Stamped.checkOrdering(1, Seq(Stamped(1, ""), Stamped(2, "")))
      == Left(Problem("Duplicate EventId 1/1970-01-01T00:00:00.000Z-001")))
    assert(Stamped.checkOrdering(3, Seq(Stamped(4, ""), Stamped(2, "")))
      == Left(Problem("EventId 2/1970-01-01T00:00:00.000Z-002 <= 4/1970-01-01T00:00:00.000Z-004")))
  }
}
