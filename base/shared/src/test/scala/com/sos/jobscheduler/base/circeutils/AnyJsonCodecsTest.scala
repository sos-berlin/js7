package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.circeutils.AnyJsonCodecs._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
final class AnyJsonCodecsTest extends FreeSpec {

  "anyToJson" in {
    assert(anyToJson(Map("key" -> 333).asJava) == Json.fromJsonObject(JsonObject.fromMap(Map("key" -> 333.asJson))))
    assert(anyToJson(Vector(111, 222).asJava) == Json.fromValues(Vector(111.asJson, 222.asJson)))
    assert(anyToJson(new java.math.BigDecimal("123.456")) == Json.fromBigDecimal(BigDecimal("123.456")))
    case class A()
    intercept[RuntimeException] {
      anyToJson(A())
    }
  }

  "anyToJson, jsonToAny" in {
    val map = Map(
      "string" -> "STRING",
      "int" -> 333,
      "boolean" -> true,
      "array" -> Vector(1, "two"))
    val json =
      json"""{
        "string": "STRING",
        "int": 333,
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
  }
}
