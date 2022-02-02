package js7.data.order

import js7.base.circeutils.CirceUtils._
import js7.base.io.process.ReturnCode
import js7.base.problem.Problem
import js7.data.order.Outcome.Completed
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OutcomeTest extends AnyFreeSpec
{
  "isSucceeded" in {
    assert(Outcome.Succeeded(NamedValues.rc(0)).isSucceeded)
    assert(Outcome.Succeeded(NamedValues.rc(1)).isSucceeded)
    assert(!Outcome.Disrupted(Problem("error")).isSucceeded)
    assert(Outcome.Disrupted(Problem("error")) == Outcome.Disrupted(Problem("error")))
    assert(!Outcome.Disrupted(Outcome.Disrupted.ProcessLost).isSucceeded)
  }

  "Completed" in {
    assert(Completed(true, Map("returnCode" -> NumberValue(1), "K" -> StringValue("V"))) == Outcome.Succeeded(Map("returnCode" -> NumberValue(1), "K" -> StringValue("V"))))
    assert(Completed(false, Map("returnCode" -> NumberValue(1), "K" -> StringValue("V"))) == Outcome.Failed(Map("returnCode" -> NumberValue(1), "K" -> StringValue("V"))))
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
      testJson[Outcome](Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "KEY" -> StringValue("VALUE"))), json"""
        {
          "TYPE": "Succeeded",
          "namedValues": {
            "returnCode": 0,
            "KEY": "VALUE"
          }
        }""")
    }

    "Failed" in {
      testJson[Outcome](Outcome.Failed(None, Map("returnCode" -> NumberValue(1))), json"""
        {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          }
        }""")
    }

    "Failed complete" in {
      testJson[Outcome](Outcome.Failed(Some("ERROR"), Map("returnCode" -> NumberValue(1), "KEY" -> StringValue("VALUE"))), json"""
        {
          "TYPE": "Failed",
          "message": "ERROR",
          "namedValues": {
            "returnCode": 1,
            "KEY": "VALUE"
          }
        }""")
    }

    "TimedOut with Failed" in {
      testJson[Outcome](Outcome.TimedOut(Outcome.Failed(Map("returnCode" -> NumberValue(128+15)))), json"""
        {
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
      testJson[Outcome](Outcome.Killed(Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "KEY" -> StringValue("VALUE")))), json"""
        {
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
      testJson[Outcome](Outcome.Disrupted(Outcome.Disrupted.ProcessLost), json"""
        {
          "TYPE": "Disrupted",
          "reason": {
            "TYPE": "ProcessLost"
          }
        }""")

      // COMPATIBLE with v2.2
      testJsonDecoder[Outcome](Outcome.Disrupted(Outcome.Disrupted.ProcessLost), json"""
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
    assert(Outcome.Succeeded.rc(ReturnCode(0)) eq Outcome.Completed(true, Map("returnCode" -> NumberValue(0))))
  }
}
