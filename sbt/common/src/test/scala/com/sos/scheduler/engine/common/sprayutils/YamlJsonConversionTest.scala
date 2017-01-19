package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.YamlJsonConversion._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class YamlJsonConversionTest extends FreeSpec {

  private val jsObject = JsObject(
    "number" → JsNumber(1.23),
    "string" → JsString("STRING"),
    "array" → JsArray(
      JsNumber(1),
      JsBoolean(true),
      JsObject("x" → JsString("X")),
      JsArray(JsNumber(2), JsNull)),
    "nested" → JsObject(
      "a" → JsBoolean(true)))
  private val yaml = """number: 1.23
                       |string: STRING
                       |array:
                       |- 1
                       |- true
                       |- x: X
                       |- - 2
                       |  - null
                       |nested:
                       |  a: true
                       |""".stripMargin

  "toYaml" in {
    assert(toYaml(jsObject) == yaml)
  }

  ".toYaml" in {
    case class A(x: Int, y: String)
    implicit val jsonFormat = jsonFormat2(A.apply)
    assert(A(123, "ABC").toYaml == "x: 123\n" + "y: ABC\n")
  }

  ".toJson" in {
    assert(yamlToJsValue(yaml) == jsObject)
  }

  ".toFlowYaml" in {
    case class A(string: String, number: Int, `"quoted`: Boolean)
    implicit val jsonFormat = jsonFormat3(A.apply)
    val a = A("test", 1, true)

    assert(a.toFlowYaml == """{string: test, number: 1, '"quoted': true}""")
  }
}
