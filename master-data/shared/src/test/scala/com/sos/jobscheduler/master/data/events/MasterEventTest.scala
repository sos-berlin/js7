package js7.master.data.events

import js7.base.circeutils.CirceUtils._
import js7.master.data.events.MasterEvent._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterEventTest extends AnyFreeSpec
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
    testJson[MasterEvent](MasterShutDown(),
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

