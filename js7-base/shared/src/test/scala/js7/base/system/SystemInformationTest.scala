package js7.base.system

import js7.base.circeutils.CirceUtils.*
import js7.tester.CirceJsonTester.testJson
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
