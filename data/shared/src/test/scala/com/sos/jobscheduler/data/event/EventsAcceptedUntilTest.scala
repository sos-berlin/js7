package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventsAcceptedUntilTest extends FreeSpec {

  "JSON" in {
    testJson(EventsAcceptedUntil(untilEventId = 1000),
      json"""{
        "untilEventId": 1000
      }""")
  }
}
