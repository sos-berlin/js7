package js7.data.controller

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.{Timestamp, Timezone}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

/**
  * @author Joacim Zschimmer
  */
final class ControllerMetaStateTest extends OurTestSuite:
  "ControllerMetaState" in:
    implicit val x = ControllerState.snapshotObjectJsonCodec
    testJson[Any](ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      Timestamp("2019-05-24T12:00:00Z"),
      Timezone("Europe/Berlin")),
      json"""{
        "TYPE": "ControllerMetaState",
        "controllerId": "CONTROLLER-ID",
        "initiallyStartedAt": 1558699200000,
        "timezone": "Europe/Berlin"
      }""")

    // COMPATIBLE with 2.0
    testJsonDecoder[Any](ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      Timestamp("2019-05-24T12:00:00Z"),
      Timezone("Europe/Berlin")),
      json"""{
        "TYPE": "ControllerMetaState",
        "controllerId": "CONTROLLER-ID",
        "startedAt": 1558699200000,
        "timezone": "Europe/Berlin"
      }""")
