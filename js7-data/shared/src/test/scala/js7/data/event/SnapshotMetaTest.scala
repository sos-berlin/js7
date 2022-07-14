package js7.data.event

import js7.base.circeutils.CirceUtils.*
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.tester.CirceJsonTester.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SnapshotMetaTest extends AnyFreeSpec
{
  "SnapshotEventId" in {
    testJson[SnapshotMeta](SnapshotEventId(1000L),
      json"""{
        "TYPE": "SnapshotEventId",
        "eventId": 1000
      }""")
  }
}
