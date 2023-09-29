package js7.base.circeutils

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import js7.base.circeutils.AnyJsonCodecs.*
import js7.base.circeutils.AnyJsonCodecsTest.*
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString
import js7.base.test.OurTestSuite
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final class AnyJsonCodecsTest extends OurTestSuite:
  "anyToJson" in:
    assert(anyToJson(Map("key" -> 333).asJava) == Json.fromJsonObject(JsonObject.fromMap(Map("key" -> 333.asJson))))
    assert(anyToJson(Vector(111, 222).asJava) == Json.fromValues(Vector(111.asJson, 222.asJson)))
    assert(anyToJson(new java.math.BigDecimal("123.456")) == Json.fromBigDecimal(BigDecimal("123.456")))
    case class A()
    intercept[RuntimeException]:
      anyToJson(A())

  "anyToJson, jsonToAny" in:
    val map = Map[String, Any](
      "string" -> "STRING",
      "int" -> 333,
      "bigDecimal" -> BigDecimal("111222333444555666777888999.1234556789"),
      "boolean" -> true,
      "array" -> Vector(1, "two"))
    val json =
      json"""{
        "string": "STRING",
        "int": 333,
        "bigDecimal": 111222333444555666777888999.1234556789,
        "boolean": true,
        "array": [1, "two"]
      }"""
    assert(mapToJson(map) == json)
    assert(jsonToAny(json) == map)

    assert(jsonToAny(Json.fromInt(Int.MinValue)) == Int.MinValue)
    assert(jsonToAny(Json.fromInt(Int.MaxValue)) == Int.MaxValue)
    assert(jsonToAny(Json.fromLong(Long.MaxValue)) == Long.MaxValue)
    assert(jsonToAny(Json.fromDoubleOrNull(1.23456789)) == 1.23456789)
    assert(jsonToAny(Json.fromBigDecimal(BigDecimal("111222333444555666777888999001"))) == BigDecimal("111222333444555666777888999001"))

  "GenericString" in:
    val map = Map(
      "string" -> "STRING",
      "wrapped" -> WrappedString("WRAPPED"))
    val json =
      json"""{
        "string": "STRING",
        "wrapped": "WRAPPED"
      }"""
    assert(mapToJson(map) == json)

private object AnyJsonCodecsTest:
  private final case class WrappedString(string: String) extends GenericString
