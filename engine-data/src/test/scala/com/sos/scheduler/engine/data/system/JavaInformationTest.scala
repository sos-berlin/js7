package com.sos.scheduler.engine.data.system

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JavaInformationTest extends FreeSpec {

  "JSON 1" in {
    val obj = JavaInformation(
      systemProperties = Map("test" → "TEST"),
      JavaInformation.Memory(maximum = 3, total = 2, free = 1))
    val json = s"""{
      "systemProperties": {
        "test": "TEST"
      },
      "memory": {
        "maximum": 3,
        "total": 2,
        "free": 1
      }
    }""".parseJson
    assert(obj.toJson == json)
  }

  "JSON 2" in {
    val obj = JavaInformation()
    val json = obj.toJson.asJsObject
    assert(json.convertTo[JavaInformation] == obj)
    assert(json.fields("systemProperties").asJsObject.fields contains "java.version")
    assert(json.fields("systemProperties").asJsObject.fields contains "os.arch")
  }
}
