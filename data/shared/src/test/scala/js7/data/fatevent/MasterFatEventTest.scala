package js7.data.fatevent

import js7.base.circeutils.CirceUtils._
import js7.data.fatevent.MasterFatEvent._
import js7.data.master.MasterId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterFatEventTest extends AnyFreeSpec
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
