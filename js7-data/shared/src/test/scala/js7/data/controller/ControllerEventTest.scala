package js7.data.controller

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.{Timestamp, Timezone}
import js7.data.controller.ControllerEvent.*
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class ControllerEventTest extends OurTestSuite:
  "ControllerReady" in:
    testJson[ControllerEvent](ControllerReady(Timezone("Europe/Berlin"), 1.hour),
      json"""{
        "TYPE": "ControllerReady",
        "timezone": "Europe/Berlin",
        "totalRunningTime": 3600
      }""")

  "ControllerInitialized" in:
    testJson[ControllerEvent](ControllerInitialized(
      ControllerId("CONTROLLER"),
      Timestamp("2021-10-27T12:00:00Z")),
      json"""{
        "TYPE": "ControllerInitialized",
        "controllerId": "CONTROLLER",
        "initiallyStartedAt": 1635336000000
      }""")

    // COMPATIBLE with 2.0
    testJsonDecoder[ControllerEvent](ControllerInitialized(
      ControllerId("CONTROLLER"),
      Timestamp("2021-10-27T12:00:00Z")),
      json"""{
        "TYPE": "ControllerInitialized",
        "controllerId": "CONTROLLER",
        "startedAt": 1635336000000
      }""")

  "ControllerShutDown" in:
    testJson[ControllerEvent](ControllerShutDown,
      json"""{
        "TYPE": "ControllerShutDown"
      }""")

  "ControllerTestEvent" in:
    testJson[ControllerEvent](ControllerTestEvent,
      json"""{
        "TYPE": "ControllerTestEvent"
      }""")
