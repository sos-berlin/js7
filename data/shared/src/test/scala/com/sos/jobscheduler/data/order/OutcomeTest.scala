package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OutcomeTest extends FreeSpec {

  "isSuccess" in {
    assert(Outcome.Good(returnValue = true).isSuccess)
    assert(!Outcome.Good(returnValue = false).isSuccess)
    assert(!Outcome.Bad(Outcome.Bad.Other("error")).isSuccess)
    assert(Outcome.Bad(Outcome.Bad.Other("error")) == Outcome.Bad("error"))
    assert(!Outcome.Bad(Outcome.Bad.AgentAborted).isSuccess)
  }

  "JSON" - {
    "Good" in {
      testJson[Outcome](Outcome.Good(true),"""{
        "TYPE": "Good",
        "returnValue": true
      }""")
    }

    "Bad(AgenAborted)" in {
      testJson[Outcome](Outcome.Bad(Outcome.Bad.AgentAborted),"""{
        "TYPE": "Bad",
        "reason": {
          "TYPE": "AgentAborted"
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
