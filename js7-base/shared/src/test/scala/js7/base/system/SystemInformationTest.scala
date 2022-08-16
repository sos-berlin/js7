package js7.base.system

import js7.base.circeutils.CirceUtils.*
import js7.base.test.Test
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationTest extends Test {

  "JSON 1" in {
    testJson(SystemInformation.ForTest,
      json"""{
        "hostname": "HOSTNAME",
        "mxBeans": {}
      }""")
  }
}
