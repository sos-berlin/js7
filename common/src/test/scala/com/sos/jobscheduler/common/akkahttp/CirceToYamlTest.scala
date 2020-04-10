package com.sos.jobscheduler.common.akkahttp

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.common.http.CirceToYaml._
import io.circe.{Json, JsonObject}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CirceToYamlTest extends AnyFreeSpec {

  private val json = Json.fromJsonObject(JsonObject.fromMap(Map[String, Json](
    "number" -> Json.fromDoubleOrNull(1.23),
    "string" -> Json.fromString("STRING"),
    "array" -> Json.fromValues(List(
      Json.fromLong(-1112223334445556667L),
      Json.fromBoolean(true),
      Json.fromJsonObject(JsonObject.fromMap(Map("x" -> Json.fromString("X")))),
      Json.fromValues(List(Json.fromInt(2), Json.Null)))),
    "nested" -> Json.fromJsonObject(JsonObject.fromMap(Map(
      "a" -> Json.fromBoolean(true)))))))
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
    assert(json.toYamlString == yaml)
  }

  "yamlToJson" in {
    assert(yamlToJson(yaml) == Right(json))
  }

  "yamlToJson, failed" in {
    val Left(problem: YamlProblem) = yamlToJson("{ INVALID YAML")
    assert(problem.toString ==
      """YAML error: while parsing a flow mapping
        | in 'string', line 1, column 1:
        |    { INVALID YAML
        |    ^
        |expected ',' or '}', but got <stream end>
        | in 'string', line 1, column 15:
        |    { INVALID YAML
        |                  ^"""
        .stripMargin)
    assert(problem.line == 0)
    assert(problem.column == 14)
  }

  ".toFlowYamlString" in {
    case class A(string: String, number: Int, `"quoted`: Boolean)
    implicit val jsonCodec = deriveCodec[A]
    val a = A("test", 1, true)

    assert(a.toFlowYamlString == """{string: test, number: 1, '"quoted': true}""")
  }
}
