package com.sos.jobscheduler.base.system

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationTest extends FreeSpec {

  "JSON 1" in {
    testJson(SystemInformation.ForTest,
      json"""{
        "hostname": "HOSTNAME",
        "mxBeans": {}
      }""")
  }
}
