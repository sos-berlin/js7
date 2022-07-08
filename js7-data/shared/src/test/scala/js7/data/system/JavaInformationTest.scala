package js7.data.system

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaInformationTest extends AnyFreeSpec {

  "JSON 1" in {
    testJson(JavaInformation(
      version = "x.y.z",
      availableProcessors = 8,
      JavaInformation.Memory(maximum = 3, total = 2, free = 1),
      systemProperties = Map("test" -> "TEST")),
    json"""{
      "version": "x.y.z",
      "memory": {
        "maximum": 3,
        "total": 2,
        "free": 1
      },
      "availableProcessors": 8,
      "systemProperties": {
        "test": "TEST"
      }
    }""")
  }
}
