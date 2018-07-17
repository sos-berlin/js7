package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.event.JournalStateEvent.EventsAccepted
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalStateEventTest extends FreeSpec
{
  "JSON" in {
    testJson[JournalStateEvent](EventsAccepted(untilEventId = 1000),
      json"""{
        "TYPE": "EventsAccepted",
        "untilEventId": 1000
      }""")
  }
}
