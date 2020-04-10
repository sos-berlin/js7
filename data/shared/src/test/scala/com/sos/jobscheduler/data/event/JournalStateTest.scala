package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      JournalState(Map(UserId("A-USER") -> EventId(1000))),
      json"""{
      "userIdToReleasedEventId": {
        "A-USER": 1000
      }
    }""")
  }
}
