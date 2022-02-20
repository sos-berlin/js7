package js7.data.delegate

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class DelegateCouplingStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[DelegateCouplingState](
      DelegateCouplingState.Reset,
      json"""{
        "TYPE": "Reset"
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
