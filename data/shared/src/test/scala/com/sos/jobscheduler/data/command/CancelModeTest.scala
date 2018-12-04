package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CancelModeTest extends FreeSpec
{
  "JSON" - {
    "NotStarted" in {
      testJson[CancelMode](CancelMode.NotStarted,
        json"""{
          "TYPE": "NotStarted"
         }""")
    }

    "FreshOrStarted" in {
      testJson[CancelMode](CancelMode.FreshOrStarted,
        json"""{
          "TYPE": "FreshOrStarted"
        } """)
    }
  }
}
