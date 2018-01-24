package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OutcomeTest extends FreeSpec {

  "isSuccess" in {
    assert(Outcome.Good(ReturnCode(0)).isSuccess)
    assert(Outcome.Good(ReturnCode(1)).isError)
    assert(!Outcome.Bad(Outcome.Bad.Other("error")).isSuccess)
    assert(Outcome.Bad(Outcome.Bad.Other("error")) == Outcome.Bad("error"))
    assert(!Outcome.Bad(Outcome.Bad.AgentRestarted).isSuccess)
  }

  "JSON" - {
    "Good" in {
      testJson[Outcome](Outcome.Good(ReturnCode(0)),"""{
        "TYPE": "Good",
        "returnCode": 0
      }""")
    }

    "Bad(AgenAborted)" in {
      testJson[Outcome](Outcome.Bad(Outcome.Bad.AgentRestarted),"""{
        "TYPE": "Bad",
        "reason": {
          "TYPE": "AgentRestarted"
        }
      }""")
    }

    "Bad(Other)" in {
      testJson[Outcome](Outcome.Bad(Outcome.Bad.Other("OTHER")),"""{
        "TYPE": "Bad",
        "reason": {
          "TYPE": "Other",
          "message": "OTHER"
        }
      }""")
    }
  }
}
