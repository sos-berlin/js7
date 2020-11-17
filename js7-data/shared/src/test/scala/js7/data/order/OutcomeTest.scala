package js7.data.order

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.job.ReturnCode
import js7.data.order.Outcome.Completed
import js7.data.value.{NamedValues, NumericValue, StringValue}
import js7.tester.CirceJsonTester.testJson
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
    assert(!Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted).isSucceeded)
  }

  "Completed" in {
    assert(Completed(true, Map("returnCode" -> NumericValue(1), "K" -> StringValue("V"))) == Outcome.Succeeded(Map("returnCode" -> NumericValue(1), "K" -> StringValue("V"))))
    assert(Completed(false, Map("returnCode" -> NumericValue(1), "K" -> StringValue("V"))) == Outcome.Failed(Map("returnCode" -> NumericValue(1), "K" -> StringValue("V"))))
    assert((Outcome.Disrupted(Problem("PROBLEM")): Outcome) match {
      case _: Outcome.Completed => false
      case _ => true
    })
  }

  "JSON" - {
    "Succeeded" in {
      testJson[Outcome](Outcome.Succeeded(Map("returnCode" -> NumericValue(0))), json"""
        {
          "TYPE": "Succeeded",
          "namedValues": {
            "returnCode": 0
          }
        }""")
    }

    "Succeeded with namedValues" in {
      testJson[Outcome](Outcome.Succeeded(Map("returnCode" -> NumericValue(0), "KEY" -> StringValue("VALUE"))), json"""
        {
          "TYPE": "Succeeded",
          "namedValues": {
            "returnCode": 0,
            "KEY": "VALUE"
          }
        }""")
    }

    "Failed" in {
      testJson[Outcome](Outcome.Failed(None, Map("returnCode" -> NumericValue(1))), json"""
        {
          "TYPE": "Failed",
          "namedValues": {
            "returnCode": 1
          }
        }""")
    }

    "Failed complete" in {
      testJson[Outcome](Outcome.Failed(Some("ERROR"), Map("returnCode" -> NumericValue(1), "KEY" -> StringValue("VALUE"))), json"""
        {
          "TYPE": "Failed",
          "message": "ERROR",
          "namedValues": {
            "returnCode": 1,
            "KEY": "VALUE"
          }
        }""")
    }

    "Cancelled with Succeeded and namedValues" in {
      testJson[Outcome](Outcome.Cancelled(Outcome.Succeeded(Map("returnCode" -> NumericValue(0), "KEY" -> StringValue("VALUE")))), json"""
        {
          "TYPE": "Cancelled",
          "outcome": {
            "TYPE": "Succeeded",
            "namedValues": {
              "returnCode": 0,
              "KEY": "VALUE"
            }
          }
        }""")
    }

    "Disrupted(JobSchedulerRestarted)" in {
      testJson[Outcome](Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted), json"""
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
    assert(Outcome.Succeeded(ReturnCode(0)) eq Outcome.Succeeded(ReturnCode(0)))
    assert(Outcome.Succeeded(ReturnCode(0)) eq Outcome.Completed(true, Map("returnCode" -> NumericValue(0))))
  }
}
