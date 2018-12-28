package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.data.events.MasterEvent._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterEventTest extends FreeSpec
{
  "MasterReady" in {
    testJson[MasterEvent](MasterReady(MasterId("MASTER"), ZoneId.of("Europe/Berlin").getId),
      json"""{
        "TYPE": "MasterReady",
        "masterId": "MASTER",
        "timezone": "Europe/Berlin"
      }""")
  }

  "MasterTestEvent" in {
    testJson[MasterEvent](MasterTestEvent,
      json"""{
        "TYPE": "MasterTestEvent"
      }""")
  }
}
