package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.data.events.MasterEvent._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.ZoneId
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterEventTest extends FreeSpec
{
  "MasterStarted" in {
    testJson[MasterEvent](MasterStarted(MasterId("MASTER-ID"), Timestamp("2019-05-24T12:00:00Z")),
      json"""{
        "TYPE": "MasterStarted",
        "masterId": "MASTER-ID",
        "startedAt": 1558699200000
      }""")
  }

  "MasterReady" in {
    testJson[MasterEvent](MasterReady(ZoneId.of("Europe/Berlin").getId, 1.hour),
      json"""{
        "TYPE": "MasterReady",
        "timezone": "Europe/Berlin",
        "totalRunningTime": 3600
      }""")
  }

  "MasterTestEvent" in {
    testJson[MasterEvent](MasterTestEvent,
      json"""{
        "TYPE": "MasterTestEvent"
      }""")
  }
}

