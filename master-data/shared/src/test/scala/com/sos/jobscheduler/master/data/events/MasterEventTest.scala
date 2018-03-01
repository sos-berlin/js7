package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.master.data.events.MasterEvent._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterEventTest extends FreeSpec
{
  "MasterReady" in {
    testJson[MasterEvent](MasterReady,
      json"""{
        "TYPE": "MasterReady"
      }""")
  }
}
