package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PayloadTest extends FreeSpec {

  "Payload" in {
    testJson(Payload(Map("VAR" → "VALUE")),"""{
        "variables": {
          "VAR": "VALUE"
        },
        "outcome": {
          "TYPE": "Good",
          "returnCode": 0
        }
      }""")
  }
}
