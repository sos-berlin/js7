package com.sos.jobscheduler.data.system

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaInformationTest extends FreeSpec {

  "JSON 1" in {
    testJson(JavaInformation(
      systemProperties = Map("test" → "TEST"),
      JavaInformation.Memory(maximum = 3, total = 2, free = 1)),
    s"""{
      "systemProperties": {
        "test": "TEST"
      },
      "memory": {
        "maximum": 3,
        "total": 2,
        "free": 1
      }
    }""")
  }

  "JSON 2" in {
    val javaInformation = JavaInformation()
    val json = javaInformation.asJson
    assert(json.as[JavaInformation] == Right(javaInformation))
    assert(json.forceObject.toMap("systemProperties").forceObject.toMap contains "java.version")
    assert(json.forceObject.toMap("systemProperties").forceObject.toMap contains "os.arch")
  }
}
