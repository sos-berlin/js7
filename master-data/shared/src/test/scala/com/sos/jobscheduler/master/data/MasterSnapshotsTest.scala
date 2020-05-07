package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
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
