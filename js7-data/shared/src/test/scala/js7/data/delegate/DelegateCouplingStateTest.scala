package js7.data.delegate

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

final class DelegateCouplingStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJsonDecoder[DelegateCouplingState](
      DelegateCouplingState.Reset.byCommand,
      json"""{
        "TYPE": "Reset"
      }""")

    testJson[DelegateCouplingState](
      DelegateCouplingState.Reset.fresh,
      json"""{
        "TYPE": "Reset",
        "reason": {
          "TYPE": "Fresh"
        }
      }""")

    testJson[DelegateCouplingState](
      DelegateCouplingState.Reset.byCommand,
      json"""{
        "TYPE": "Reset",
        "reason": {
          "TYPE": "ResetCommand"
        }
      }""")

    testJson[DelegateCouplingState](
      DelegateCouplingState.Reset.shutdown,
      json"""{
        "TYPE": "Reset",
        "reason": {
          "TYPE": "Shutdown"
        }
      }""")

    testJson[DelegateCouplingState](
      DelegateCouplingState.Reset.restart,
      json"""{
        "TYPE": "Reset",
        "reason": {
          "TYPE": "Restart"
        }
      }""")

    testJson[DelegateCouplingState](
      DelegateCouplingState.Resetting(force = false),
      json"""{
        "TYPE": "Resetting",
        "force": false
      }""")

    testJson[DelegateCouplingState](
      DelegateCouplingState.Coupled,
      json"""{
        "TYPE": "Coupled"
      }""")

    testJson[DelegateCouplingState](
      DelegateCouplingState.ShutDown,
      json"""{
        "TYPE": "ShutDown"
      }""")
  }
}
