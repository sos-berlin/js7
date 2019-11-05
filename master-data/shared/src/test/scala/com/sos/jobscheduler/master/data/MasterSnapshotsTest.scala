package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterSnapshotsTest extends FreeSpec
{
  "MasterMetaState" in {
    implicit val x = MasterSnapshots.SnapshotJsonCodec
    testJson[Any](MasterMetaState(MasterId("MASTER-ID"), Timestamp("2019-05-24T12:00:00Z")),
      json"""{
        "TYPE": "MasterMetaState",
        "masterId": "MASTER-ID",
        "startedAt": 1558699200000
      }""")
  }
}
