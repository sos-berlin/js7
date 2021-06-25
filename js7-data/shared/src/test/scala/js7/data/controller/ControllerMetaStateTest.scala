package js7.data.controller

import js7.base.circeutils.CirceUtils._
import js7.base.time.Timestamp
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerMetaStateTest extends AnyFreeSpec
{
  "ControllerMetaState" in {
    java.time.ZoneId.of("Europe/Berlin")  // Proper timezone
    implicit val x = ControllerState.snapshotObjectJsonCodec
    testJson[Any](ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      Timestamp("2019-05-24T12:00:00Z"),
      "Europe/Berlin"),
      json"""{
        "TYPE": "ControllerMetaState",
        "controllerId": "CONTROLLER-ID",
        "startedAt": 1558699200000,
        "timezone": "Europe/Berlin"
      }""")
  }
}
