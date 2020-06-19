package js7.controller.data.events

import js7.base.circeutils.CirceUtils._
import js7.controller.data.events.ControllerEvent._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ControllerEventTest extends AnyFreeSpec
{
  "ControllerReady" in {
    testJson[ControllerEvent](ControllerReady("Europe/Berlin", 1.hour),
      json"""{
        "TYPE": "ControllerReady",
        "timezone": "Europe/Berlin",
        "totalRunningTime": 3600
      }""")
  }

  "ControllerShutDown" in {
    testJson[ControllerEvent](ControllerShutDown(),
      json"""{
        "TYPE": "ControllerShutDown"
      }""")
  }

  "ControllerTestEvent" in {
    testJson[ControllerEvent](ControllerTestEvent,
      json"""{
        "TYPE": "ControllerTestEvent"
      }""")
  }
}
