package js7.data.order

import js7.base.circeutils.CirceUtils.*
import js7.base.io.process.ReturnCode
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.order.OrderOutcome.Completed
import js7.data.subagent.Problems.{ProcessLostDueToRestartProblem, ProcessLostDueToUnknownReasonProblem}
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

/**
  * @author Joacim Zschimmer
  */
final class OrderOutcomeTest extends OurTestSuite
{
  "isSucceeded" in {
    assert(OrderOutcome.Succeeded(NamedValues.rc(0)).isSucceeded)
    assert(OrderOutcome.Succeeded(NamedValues.rc(1)).isSucceeded)
    assert(!OrderOutcome.Disrupted(Problem("error")).isSucceeded)
    assert(OrderOutcome.Disrupted(Problem("error")) == OrderOutcome.Disrupted(Problem("error")))
  }

  "Completed" in {
    val namedValues = Map(
      "returnCode" -> NumberValue(1),
      "K" -> StringValue("V"))

    assert(Completed(true, namedValues) == OrderOutcome.Succeeded(namedValues))
    assert(Completed(false, namedValues) == OrderOutcome.Failed(namedValues))
    assert((OrderOutcome.Disrupted(Problem("PROBLEM")): OrderOutcome) match {
      case _: OrderOutcome.Completed => false
      case _ => true
    })
  }

  "JSON" - {
    "Succeeded" in {
      testJson[OrderOutcome](OrderOutcome.Succeeded(Map("returnCode" -> NumberValue(0))), json"""
        {
          "TYPE": "Succeeded",
          "namedValues": {
            "returnCode": 0
          }
        }""")
    }

    "Succeeded with namedValues" in {
      testJson[OrderOutcome](
        OrderOutcome.Succeeded(
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
      testJson[OrderOutcome](OrderOutcome.failed, json"""
        {
          "TYPE": "Failed"
        }""")

      testJsonDecoder[OrderOutcome](OrderOutcome.failed, json"""
        {
          "TYPE": "Failed",
          "namedValues": {}
        }""")

      testJsonDecoder[OrderOutcome](OrderOutcome.Failed(None, Map.empty, uncatchable = true), json"""
        {
          "TYPE": "Failed",
          "uncatchable": true,
          "namedValues": {}
        }""")
    }

    "Failed complete" in {
      testJson[OrderOutcome](
        OrderOutcome.Failed(
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
      testJson[OrderOutcome](
        OrderOutcome.TimedOut(
          OrderOutcome.Failed(
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
      testJson[OrderOutcome](
        OrderOutcome.Killed(
          OrderOutcome.Succeeded(
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
      testJson[OrderOutcome](OrderOutcome.processLost(ProcessLostDueToRestartProblem), json"""
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

      testJson[OrderOutcome](OrderOutcome.processLost(ProcessLostDueToRestartProblem), json"""
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
      testJsonDecoder[OrderOutcome](OrderOutcome.processLost(ProcessLostDueToUnknownReasonProblem), json"""
        {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "JobSchedulerRestarted"
          }
        }""")
    }

    "Disrupted(Other)" in {
      testJson[OrderOutcome](OrderOutcome.Disrupted(Problem("OTHER"), uncatchable = true), json"""
        {
          "TYPE": "Disrupted",
          "uncatchable": true,
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
    assert(OrderOutcome.Succeeded(NamedValues.empty) eq OrderOutcome.succeeded)
    assert(OrderOutcome.Succeeded(NamedValues.empty) eq OrderOutcome.Completed(true))
    assert(OrderOutcome.Succeeded.rc(0) eq OrderOutcome.Succeeded.rc(ReturnCode(0)))
    assert(OrderOutcome.Succeeded.rc(ReturnCode(0)) eq
      OrderOutcome.Completed(true, Map("returnCode" -> NumberValue(0))))
  }
}
