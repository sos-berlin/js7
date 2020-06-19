package js7.data.fatevent

import js7.base.circeutils.CirceUtils._
import js7.data.controller.ControllerId
import js7.data.fatevent.ControllerFatEvent._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerFatEventTest extends AnyFreeSpec
{
  "ControllerReadyFat" in {
    testJson[ControllerFatEvent](ControllerReadyFat(ControllerId("CONTROLLER"), "Europe/Berlin"),
      json"""{
        "TYPE": "ControllerReadyFat",
        "controllerId": "CONTROLLER",
        "timezone": "Europe/Berlin"
      }""")
  }
}
