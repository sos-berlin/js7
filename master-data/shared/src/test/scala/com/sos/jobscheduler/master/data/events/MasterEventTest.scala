package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.master.data.events.MasterEvent._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterEventTest extends FreeSpec
{
  "MasterReady" in {
    testJson[MasterEvent](MasterReady("Europe/Berlin", 1.hour),
      json"""{
        "TYPE": "MasterReady",
        "timezone": "Europe/Berlin",
        "totalRunningTime": 3600
      }""")
  }

  "MasterShutDown" in {
    testJson[MasterEvent](MasterShutDown,
      json"""{
        "TYPE": "MasterShutDown"
      }""")
  }

  "MasterTestEvent" in {
    testJson[MasterEvent](MasterTestEvent,
      json"""{
        "TYPE": "MasterTestEvent"
      }""")
  }
}

