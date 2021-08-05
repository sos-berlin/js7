package js7.data.value

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ValueTest extends AnyFreeSpec
{
  private val bigNumber = "111222333444555666777888999.9998887776665555444333222111e99"

  "JSON" - {
    "Values" in {
      testJson[Value](StringValue("X"),
        json""" "X" """)
      testJson[Value](NumberValue(BigDecimal(bigNumber)),
        json"""$bigNumber""")
      assert((NumberValue(BigDecimal(bigNumber)): Value).asJson.toString ==
        "1.112223334445556667778889999998887776665555444333222111E+125")
      testJson[Value](BooleanValue(false),
        json"""false""")
      testJson[Value](ListValue.empty,
        json"""[]""")
      testJson[Value](ListValue(List(
        StringValue("STRING"), NumberValue(1), BooleanValue.True, ListValue(List(BooleanValue.False)))),
        json"""[ "STRING", 1, true, [ false ]] """)
      testJson[Value](ObjectValue(Map("A" -> StringValue("STRING"))),
        json"""{ "A": "STRING" }""")
    }

    "ValueType" - {
      "String" in {
        testJson[ValueType](StringValue, json""" "String" """)
      }

      "Number" in {
        testJson[ValueType](NumberValue, json""" "Number" """)
      }

      "Boolean" in {
        testJson[ValueType](BooleanValue, json""" "Boolean" """)
      }

      "List" in {
        testJson[ValueType](
          ListType(StringValue),
          json"""
            {
              "TYPE": "List",
              "elementType": "String"
            }""")
      }

      "Object" in {
        testJson[ValueType](
          ObjectType(Map("a" -> StringValue, "b" -> NumberValue)),
          json"""
            {
              "TYPE": "Object",
              "a": "String",
              "b": "Number"
            }""")
      }

      "Nested" in {
        testJson[ValueType](
          ListType(ObjectType(Map("a" -> NumberValue, "stringList" -> ListType(StringValue)))),
          json"""
            {
              "TYPE": "List",
              "elementType": {
                "TYPE": "Object",
                "a": "Number",
                "stringList": {
                  "TYPE": "List",
                  "elementType": "String"
                }
              }
            }""")
      }
    }
  }

  "StringValue" - {
    "StringValue.toStringValue" in {
      assert(StringValue("X").toStringValue == Right(StringValue("X")))
    }

    "StringValue.toNumber" in {
      assert(StringValue("X").toNumberValue.isLeft)
      assert(StringValue("0").toNumberValue == Right(NumberValue(0)))
      assert(StringValue("009").toNumberValue == Right(NumberValue(9)))
      assert(StringValue(bigNumber).toNumberValue == Right(NumberValue(BigDecimal(bigNumber))))
    }

    "StringValue.toBoolean" in {
      assert(StringValue("X").toBooleanValue.isLeft)

      assert(StringValue("true").toBooleanValue == Right(BooleanValue(true)))
      assert(StringValue("false").toBooleanValue == Right(BooleanValue(false)))
    }
  }

  "NumberValue" - {
    "NumberValue.toStringValue" in {
      assert(NumberValue(7).toStringValue == Right(StringValue("7")))
      assert(NumberValue(BigDecimal("1.2e99")).toStringValue == Right(StringValue("1.2E+99")))
      assert(NumberValue(BigDecimal(bigNumber)).toStringValue ==
        Right(StringValue("1.112223334445556667778889999998887776665555444333222111E+125")))
    }

    "NumberValue.toNumber" in {
      assert(NumberValue(7).toNumberValue == Right(NumberValue(7)))
    }

    "NumberValue.toBoolean" in {
      assert(NumberValue(1).toBooleanValue == Right(BooleanValue(true)))
      assert(NumberValue(0).toBooleanValue == Right(BooleanValue(false)))
      assert(NumberValue(-1).toBooleanValue.isLeft)
    }
  }

  "BooleanValue" - {
    "BooleanValue.toStringValue" in {
      assert(BooleanValue(true).toStringValue == Right(StringValue("true")))
      assert(BooleanValue(false).toStringValue == Right(StringValue("false")))
    }

    "BooleanValue.toNumber" in {
      assert(BooleanValue(true).toNumberValue == Right(NumberValue(1)))
      assert(BooleanValue(false).toNumberValue == Right(NumberValue(0)))
    }

    "BooleanValue.toBoolean" in {
      assert(BooleanValue(true).toBooleanValue == Right(BooleanValue(true)))
      assert(BooleanValue(false).toBooleanValue == Right(BooleanValue(false)))
    }
  }
}
