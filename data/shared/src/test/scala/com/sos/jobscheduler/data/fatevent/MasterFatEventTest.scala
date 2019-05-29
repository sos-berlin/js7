package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.fatevent.MasterFatEvent._
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterFatEventTest extends FreeSpec
{
  "MasterReadyFat" in {
    testJson[MasterFatEvent](MasterReadyFat(MasterId("MASTER"), "Europe/Berlin"),
      json"""{
        "TYPE": "MasterReadyFat",
        "masterId": "MASTER",
        "timezone": "Europe/Berlin"
      }""")
  }
}
