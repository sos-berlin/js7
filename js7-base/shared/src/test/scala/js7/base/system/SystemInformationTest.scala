package js7.base.system

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationTest extends OurTestSuite:

  "JSON 1" in:
    testJson(SystemInformation.ForTest,
      json"""{
        "hostname": "HOSTNAME",
        "mxBeans": {}
      }""")
