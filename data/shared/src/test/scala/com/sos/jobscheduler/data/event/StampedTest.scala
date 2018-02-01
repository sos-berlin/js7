package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

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
    implicit val codec = deriveCodec[A]
    testJson(Stamped(EventId(777), A(111)),
      """{
        "eventId": 777,
        "number": 111
      }""")
  }

  "JSON with array" in {
    testJson(Stamped(EventId(777), List(111, 222)),
      """{
        "eventId": 777,
        "elements": [111, 222]
      }""")
  }
}
