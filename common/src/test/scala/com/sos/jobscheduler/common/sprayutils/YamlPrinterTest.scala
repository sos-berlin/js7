package com.sos.scheduler.engine.common.sprayutils

import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class YamlPrinterTest extends FreeSpec {

  "YAML" in {
    val jsObject = JsObject(
      "number" → JsNumber(1.23),
      "string" → JsString("STRING"),
      "array" → JsArray(
        JsNumber(1),
        JsNumber(2)),
      "nested" → JsObject(
        "a" → JsBoolean(true)))
    val yaml = """number: 1.23
                 |string: STRING
                 |array:
                 |- 1
                 |- 2
                 |nested:
                 |  a: true
                 |""".stripMargin
    assert(YamlPrinter(jsObject) == yaml)
  }
}
