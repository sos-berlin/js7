package com.sos.jobscheduler.data.system

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaInformationTest extends AnyFreeSpec {

  "JSON 1" in {
    testJson(JavaInformation(
      version = "x.y.z",
      JavaInformation.Memory(maximum = 3, total = 2, free = 1),
      systemProperties = Map("test" -> "TEST")),
    json"""{
      "version": "x.y.z",
      "memory": {
        "maximum": 3,
        "total": 2,
        "free": 1
      },
      "systemProperties": {
        "test": "TEST"
      }
    }""")
  }
}
