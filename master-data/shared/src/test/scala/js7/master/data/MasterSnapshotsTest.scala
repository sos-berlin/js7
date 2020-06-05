package js7.master.data

import js7.base.circeutils.CirceUtils._
import js7.base.time.Timestamp
import js7.data.master.MasterId
import js7.master.data.MasterSnapshots.MasterMetaState
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterSnapshotsTest extends AnyFreeSpec
{
  "MasterMetaState" in {
    java.time.ZoneId.of("Europe/Berlin")  // Proper timezone
    implicit val x = MasterSnapshots.SnapshotJsonCodec
    testJson[Any](MasterMetaState(MasterId("MASTER-ID"), Timestamp("2019-05-24T12:00:00Z"), "Europe/Berlin"),
      json"""{
        "TYPE": "MasterMetaState",
        "masterId": "MASTER-ID",
        "startedAt": 1558699200000,
        "timezone": "Europe/Berlin"
      }""")
  }
}
