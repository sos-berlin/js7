package js7.data.order

import js7.base.circeutils.CirceUtils.*
import js7.base.io.process.ReturnCode
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.order.Outcome.Completed
import js7.data.subagent.Problems.{ProcessLostDueToRestartProblem, ProcessLostDueToUnknownReasonProblem}
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

/**
  * @author Joacim Zschimmer
  */
final class OutcomeTest extends OurTestSuite
{
  "isSucceeded" in {
    assert(Outcome.Succeeded(NamedValues.rc(0)).isSucceeded)
    assert(Outcome.Succeeded(NamedValues.rc(1)).isSucceeded)
    assert(!Outcome.Disrupted(Problem("error")).isSucceeded)
    assert(Outcome.Disrupted(Problem("error")) == Outcome.Disrupted(Problem("error")))
  }

  "Completed" in {
    val namedValues = Map(
      "returnCode" -> NumberValue(1),
      "K" -> StringValue("V"))

    assert(Completed(true, namedValues) == Outcome.Succeeded(namedValues))
    assert(Completed(false, namedValues) == Outcome.Failed(namedValues))
    assert((Outcome.Disrupted(Problem("PROBLEM")): Outcome) match {
      case _: Outcome.Completed => false
      case _ => true
    })
  }

  "JSON" - {
    "Succeeded" in {
      testJson[Outcome](Outcome.Succeeded(Map("returnCode" -> NumberValue(0))), json"""
        {
          "TYPE": "Succeeded",
          "namedValues": {
            "returnCode": 0
          }
        }""")
    }

    "Succeeded with namedValues" in {
      testJson[Outcome](
        Outcome.Succeeded(
          Map(
            "returnCode" -> NumberValue(0),
            "KEY" -> StringValue("VALUE"))),
        json"""{
          "TYPE": "Succeeded",
          "namedValues": {
            "returnCode": 0,
            "KEY": "VALUE"
          }
        }""")
    }

    "Failed" in {
      testJson[Outcome](Outcome.failed, json"""
        {
          "TYPE": "Failed"
        }""")

      testJsonDecoder[Outcome](Outcome.failed, json"""
        {
          "TYPE": "Failed",
          "namedValues": {}
        }""")

      testJsonDecoder[Outcome](Outcome.Failed(None, Map.empty, uncatchable = true), json"""
        {
          "TYPE": "Failed",
          "uncatchable": true,
          "namedValues": {}
        }""")
    }

    "Failed complete" in {
      testJson[Outcome](
        Outcome.Failed(
          Some("MESSAGE"),
          Map(
            "returnCode" -> NumberValue(1),
            "KEY" -> StringValue("VALUE"))),
        json"""{
          "TYPE": "Failed",
          "message": "MESSAGE",
          "namedValues": {
            "returnCode": 1,
            "KEY": "VALUE"
          }
        }""")
    }

    "TimedOut with Failed" in {
      testJson[Outcome](
        Outcome.TimedOut(
          Outcome.Failed(
            Map(
              "returnCode" -> NumberValue(128+15)))),
        json"""{
          "TYPE": "TimedOut",
          "outcome": {
            "TYPE": "Failed",
            "namedValues": {
              "returnCode": 143
            }
          }
        }""")
    }

    "Killed with Succeeded and namedValues" in {
      testJson[Outcome](
        Outcome.Killed(
          Outcome.Succeeded(
            Map(
              "returnCode" -> NumberValue(0),
              "KEY" -> StringValue("VALUE")))),
        json"""{
          "TYPE": "Killed",
          "outcome": {
            "TYPE": "Succeeded",
            "namedValues": {
              "returnCode": 0,
              "KEY": "VALUE"
            }
          }
        }""")
    }

    "Disrupted(ProcessLost)" in {
      testJson[Outcome](Outcome.processLost(ProcessLostDueToRestartProblem), json"""
        {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "ProcessLost",
            "problem": {
              "code": "ProcessLostDueToRestart",
              "message": "ProcessLostDueToRestart"
            }
          }
        }""")

      testJson[Outcome](Outcome.processLost(ProcessLostDueToRestartProblem), json"""
        {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "ProcessLost",
            "problem": {
              "code": "ProcessLostDueToRestart",
              "message": "ProcessLostDueToRestart"
            }
          }
        }""")

      // COMPATIBLE with v2.3
      testJsonDecoder[Outcome](Outcome.processLost(ProcessLostDueToUnknownReasonProblem), json"""
        {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "JobSchedulerRestarted"
          }
        }""")
    }

    "Disrupted(Other)" in {
      testJson[Outcome](Outcome.Disrupted(Problem("OTHER")), json"""
        {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "Other",
            "problem": {
              "message": "OTHER"
            }
          }
        }""")
    }
  }

  "Constant pool for memory reuse" in {
    assert(Outcome.Succeeded(NamedValues.empty) eq Outcome.succeeded)
    assert(Outcome.Succeeded(NamedValues.empty) eq Outcome.Completed(true))
    assert(Outcome.Succeeded.rc(0) eq Outcome.Succeeded.rc(ReturnCode(0)))
    assert(Outcome.Succeeded.rc(ReturnCode(0)) eq
      Outcome.Completed(true, Map("returnCode" -> NumberValue(0))))
  }
}
