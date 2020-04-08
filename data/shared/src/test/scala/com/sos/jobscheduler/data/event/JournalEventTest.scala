package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import com.sos.jobscheduler.tester.CirceJsonTester._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalEventTest extends FreeSpec
{
  "SnapshotTaken" in {
    testJson[JournalEvent](SnapshotTaken,
      json"""{
        "TYPE": "SnapshotTaken"
      }""")
  }

  "JournalEventsReleased" in {
    testJson[JournalEvent](JournalEventsReleased(UserId("USER"), EventId(1234)),
      json"""{
        "TYPE": "JournalEventsReleased",
        "userId": "USER",
        "untilEventId": 1234
      }""")
  }
}
