package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StampedTest extends FreeSpec {

  "map" in {
    val eventId = EventId(100)
    assert((Stamped(eventId, Timestamp.ofEpochMilli(123), 3) map { _.toString }) ==
      Stamped(eventId, Timestamp.ofEpochMilli(123), "3"))
  }

  "JSON with object" in {
    case class A(number: Int)
    implicit val codec = deriveCodec[A]
    testJson(Stamped(EventId(777), Timestamp.ofEpochMilli(123), A(111)),
      """{
        "eventId": 777,
        "timestamp": 123,
        "number": 111
      }""")
    testJson(Stamped(EventId(123007), Timestamp.ofEpochMilli(123), A(111)),
      """{
        "eventId": 123007,
        "number": 111
      }""")
  }

  "JSON with array" in {
    testJson(Stamped(EventId(777), Timestamp.ofEpochMilli(123), List(111, 222)),
      """{
        "eventId": 777,
        "timestamp": 123,
        "value": [111, 222]
      }""")
    testJson(Stamped(EventId(123007), Timestamp.ofEpochMilli(123), List(111, 222)),
      """{
        "eventId": 123007,
        "value": [111, 222]
      }""")
  }

  "JSON with String" in {
    testJson(Stamped(EventId(777), Timestamp.ofEpochMilli(123), "VALUE"),
      """{
        "eventId": 777,
        "timestamp": 123,
        "value": "VALUE"
      }""")
  }
}
