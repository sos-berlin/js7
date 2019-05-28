package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
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

