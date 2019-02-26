package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Outcome.Undisrupted
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OutcomeTest extends FreeSpec {

  "isSucceeded" in {
    assert(Outcome.Succeeded(ReturnCode(0)).isSucceeded)
    assert(Outcome.Succeeded(ReturnCode(1)).isFailed)
    assert(!Outcome.Disrupted(Problem("error")).isSucceeded)
    assert(Outcome.Disrupted(Problem("error")) == Outcome.Disrupted(Problem("error")))
    assert(!Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted).isSucceeded)
  }

  "Undisrupted" in {
    assert(Undisrupted(ReturnCode(1), true) == Outcome.Succeeded(ReturnCode(1)))
    assert(Undisrupted(ReturnCode(1), false) == Outcome.Failed(ReturnCode(1)))
    assert(Outcome.Succeeded(ReturnCode(1)) match {
      case Outcome.Undisrupted(ReturnCode(1), true) => true
      case _ => false
    })
    assert(Outcome.Failed(ReturnCode(1)) match {
      case Outcome.Undisrupted(ReturnCode(1), false) => true
      case _ => false
    })
    assert((Outcome.Disrupted(Problem("PROBLEM")): Outcome) match {
      case Outcome.Undisrupted(_, _) => false
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

    "Failed" in {
      testJson[Outcome](Outcome.Failed(ReturnCode(1)), json"""
        {
          "TYPE": "Failed",
          "returnCode": 1
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
}
