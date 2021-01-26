package js7.data.value

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ValueTest extends AnyFreeSpec
{
  private val bigNumber = "111222333444555666777888999.9998887776665555444333222111e99"

  "JSON" in {
    testJson[Value](StringValue("X"),
      json""" "X" """)
    testJson[Value](NumberValue(BigDecimal(bigNumber)),
      json"""$bigNumber""")
    assert((NumberValue(BigDecimal(bigNumber)): Value).asJson.toString ==
      "1.112223334445556667778889999998887776665555444333222111E+125")
    testJson[Value](BooleanValue(false),
      json"""false""")
    testJson[Value](ListValue(Nil),
      json"""[]""")
    testJson[Value](ListValue(List(
      StringValue("STRING"), NumberValue(1), BooleanValue.True, ListValue(List(BooleanValue.False)))),
      json"""[ "STRING", 1, true, [ false ]] """)
    testJson[Value](ObjectValue(Map("A" -> StringValue("STRING"))),
      json"""{ "A": "STRING" }""")
  }

  "StringValue" - {
    "StringValue.toStringValue" in {
      assert(StringValue("X").toStringValue == Right(StringValue("X")))
    }

    "StringValue.toNumber" in {
      assert(StringValue("X").toNumber.isLeft)
      assert(StringValue("0").toNumber == Right(NumberValue(0)))
      assert(StringValue("009").toNumber == Right(NumberValue(9)))
      assert(StringValue(bigNumber).toNumber == Right(NumberValue(BigDecimal(bigNumber))))
    }

    "StringValue.toBoolean" in {
      assert(StringValue("X").toBoolean.isLeft)

      assert(StringValue("true").toBoolean == Right(BooleanValue(true)))
      assert(StringValue("false").toBoolean == Right(BooleanValue(false)))
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
      assert(NumberValue(7).toNumber == Right(NumberValue(7)))
    }

    "NumberValue.toBoolean" in {
      assert(NumberValue(1).toBoolean == Right(BooleanValue(true)))
      assert(NumberValue(0).toBoolean == Right(BooleanValue(false)))
      assert(NumberValue(-1).toBoolean.isLeft)
    }
  }

  "BooleanValue" - {
    "BooleanValue.toStringValue" in {
      assert(BooleanValue(true).toStringValue == Right(StringValue("true")))
      assert(BooleanValue(false).toStringValue == Right(StringValue("false")))
    }

    "BooleanValue.toNumber" in {
      assert(BooleanValue(true).toNumber == Right(NumberValue(1)))
      assert(BooleanValue(false).toNumber == Right(NumberValue(0)))
    }

    "BooleanValue.toBoolean" in {
      assert(BooleanValue(true).toBoolean == Right(BooleanValue(true)))
      assert(BooleanValue(false).toBoolean == Right(BooleanValue(false)))
    }
  }
}
