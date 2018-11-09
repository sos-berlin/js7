package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.job.ReturnCode
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

  "JSON" - {
    "Succeeded" in {
      testJson[Outcome](Outcome.Succeeded(ReturnCode(0)),"""{
        "TYPE": "Succeeded",
        "returnCode": 0
      }""")
    }

    "Failed" in {
      testJson[Outcome](Outcome.Failed(ReturnCode(1)),"""{
        "TYPE": "Failed",
        "returnCode": 1
      }""")
    }

    "Disrupted(AgenAborted)" in {
      testJson[Outcome](Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted),"""{
        "TYPE": "Disrupted",
        "reason": {
          "TYPE": "JobSchedulerRestarted"
        }
      }""")
    }

    "Disrupted(Other)" in {
      testJson[Outcome](Outcome.Disrupted(Problem("OTHER")),"""{
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
