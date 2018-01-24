package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PayloadTest extends FreeSpec {

  "Payload" in {
    testJson(Payload(Map("VAR" → "VALUE")), json"""
      {
        "variables": {
          "VAR": "VALUE"
        }
      }""")
  }
}
