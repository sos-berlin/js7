package com.sos.jobscheduler.base.system

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationTest extends AnyFreeSpec {

  "JSON 1" in {
    testJson(SystemInformation.ForTest,
      json"""{
        "hostname": "HOSTNAME",
        "mxBeans": {}
      }""")
  }
}
