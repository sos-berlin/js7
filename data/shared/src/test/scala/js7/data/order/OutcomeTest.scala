package js7.data.order

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.job.ReturnCode
import js7.data.order.Outcome.Undisrupted
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OutcomeTest extends AnyFreeSpec {

  "isSucceeded" in {
    assert(Outcome.Succeeded(ReturnCode(0)).isSucceeded)
    assert(Outcome.Succeeded(ReturnCode(1)).isSucceeded)
    assert(!Outcome.Disrupted(Problem("error")).isSucceeded)
    assert(Outcome.Disrupted(Problem("error")) == Outcome.Disrupted(Problem("error")))
    assert(!Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted).isSucceeded)
  }

  "Undisrupted" in {
    assert(Undisrupted(true, ReturnCode(1), Map("K" -> "V")) == Outcome.Succeeded(ReturnCode(1), Map("K" -> "V")))
    assert(Undisrupted(false, ReturnCode(1), Map("K" -> "V")) == Outcome.Failed(ReturnCode(1), Map("K" -> "V")))
    assert((Outcome.Disrupted(Problem("PROBLEM")): Outcome) match {
      case _: Outcome.Undisrupted => false
      case _ => true
    })
  }

  "JSON" - {
    "Succeeded" in {
      testJson[Outcome](Outcome.Succeeded(ReturnCode(0)), json"""
        {
          "TYPE": "Succeeded",
          "returnCode": 0
        }""")
    }

    "Succeeded with keyValues" in {
      testJson[Outcome](Outcome.Succeeded(ReturnCode(0), Map("KEY" -> "VALUE")), json"""
        {
          "TYPE": "Succeeded",
          "keyValues": {
            "KEY": "VALUE"
          },
          "returnCode": 0
        }""")
    }

    "Failed" in {
      testJson[Outcome](Outcome.Failed(None, ReturnCode(1)), json"""
        {
          "TYPE": "Failed",
          "returnCode": 1
        }""")
    }

    "Failed complete" in {
      testJson[Outcome](Outcome.Failed(Some("ERROR"), ReturnCode(1), Map("KEY" -> "VALUE")), json"""
        {
          "TYPE": "Failed",
          "message": "ERROR",
          "returnCode": 1,
          "keyValues": {
            "KEY": "VALUE"
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
    assert(Outcome.Succeeded(Map.empty[String, String]) eq Outcome.Succeeded(ReturnCode(0)))
    assert(Outcome.Succeeded(Map.empty[String, String]) eq Outcome.Undisrupted(true, ReturnCode(0)))
    for (i <- 0 to 255) {
      assert(Outcome.Succeeded(ReturnCode(i)) eq Outcome.Succeeded(ReturnCode(i)))
      assert(Outcome.Succeeded(ReturnCode(i)) eq Outcome.Undisrupted(true, ReturnCode(i)))
      assert(Outcome.Failed(ReturnCode(i)) eq Outcome.Undisrupted(false, ReturnCode(i)))
    }
    assert(Outcome.Succeeded(ReturnCode(1234)) ne Outcome.Succeeded(ReturnCode(1234)))  // Not in constant pool
  }
}
