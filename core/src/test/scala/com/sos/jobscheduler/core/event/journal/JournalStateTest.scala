package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalStateTest extends FreeSpec
{
  "JSON" in {
    testJson(
      JournalState(eventsAcceptedUntil = 123),
      json"""{
        "TYPE": "JournalState",
        "eventsAcceptedUntil": 123
      }""")
  }
}
