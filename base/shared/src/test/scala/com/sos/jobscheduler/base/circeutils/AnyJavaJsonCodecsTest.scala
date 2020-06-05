package js7.base.circeutils

import js7.base.circeutils.AnyJavaJsonCodecs._
import js7.base.circeutils.CirceUtils._
import io.circe.Json
import scala.jdk.CollectionConverters._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AnyJavaJsonCodecsTest extends AnyFreeSpec {

  "jsonToJava" in {
    val map = Map(
      "string" -> "STRING",
      "int" -> 333,
      "boolean" -> true,
      "array" -> Vector(1, "two").asJava).asJava
    val json =
      json"""{
        "string": "STRING",
        "int": 333,
        "boolean": true,
        "array": [1, "two"]
      }"""
    assert(jsonToJava(json) == map)

    assert(jsonToJava(Json.fromInt(Int.MinValue)) == Int.MinValue)
    assert(jsonToJava(Json.fromInt(Int.MaxValue)) == Int.MaxValue)
    assert(jsonToJava(Json.fromLong(Long.MaxValue)) == Long.MaxValue)
    assert(jsonToJava(Json.fromDoubleOrNull(1.23456789)) == new java.math.BigDecimal("1.23456789"))
    assert(jsonToJava(Json.fromBigDecimal(BigDecimal("111222333444555666777888999001"))) ==
      new java.math.BigDecimal("111222333444555666777888999001"))
  }
}
