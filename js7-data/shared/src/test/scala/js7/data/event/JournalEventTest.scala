package js7.data.event

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.*
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.tester.CirceJsonTester.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalEventTest extends AnyFreeSpec
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
