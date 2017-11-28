package com.sos.jobscheduler.common.akkahttp

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.common.akkahttp.CirceToYaml._
import io.circe.{Json, JsonObject}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CirceToYamlTest extends FreeSpec {

  private val json = Json.fromJsonObject(JsonObject.fromMap(Map[String, Json](
    "number" → Json.fromDoubleOrNull(1.23),
    "string" → Json.fromString("STRING"),
    "array" → Json.fromValues(List(
      Json.fromLong(-1112223334445556667L),
      Json.fromBoolean(true),
      Json.fromJsonObject(JsonObject.fromMap(Map("x" → Json.fromString("X")))),
      Json.fromValues(List(Json.fromInt(2), Json.Null)))),
    "nested" → Json.fromJsonObject(JsonObject.fromMap(Map(
      "a" → Json.fromBoolean(true)))))))
  private val yaml = """number: 1.23
                       |string: STRING
                       |array:
                       |- -1112223334445556667
                       |- true
                       |- x: X
                       |- - 2
                       |  - null
                       |nested:
                       |  a: true
                       |""".stripMargin

  "toYamlString" in {
    assert(toYamlString(json) == yaml)
  }

  ".toYamlString" in {
    case class A(x: Int, y: String)
    implicit val jsonCodec = deriveCirceCodec[A]
    assert(Set("x: 123\n" + "y: ABC\n", "y: ABC\n" + "x: 123\n") contains A(123, "ABC").toYamlString)
  }

  ".toJson" in {
    assert(yamlToJson(yaml) == json)
  }

  ".toFlowYamlString" in {
    case class A(string: String, number: Int, `"quoted`: Boolean)
    implicit val jsonCodec = deriveCirceCodec[A]
    val a = A("test", 1, true)

    assert(a.toFlowYamlString == """{string: test, number: 1, '"quoted': true}""")
  }
}
