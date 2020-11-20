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
    testJson[Value](NumericValue(BigDecimal(bigNumber)),
      json"""$bigNumber""")
    assert((NumericValue(BigDecimal(bigNumber)): Value).asJson.toString ==
      "1.112223334445556667778889999998887776665555444333222111E+125")
    testJson[Value](BooleanValue(false),
      json"""false""")
    testJson[Value](ListValue(Nil),
      json"""[]""")
    testJson[Value](ListValue(List(
      StringValue("STRING"), NumericValue(1), BooleanValue.True, ListValue(List(BooleanValue.False)))),
      json"""[ "STRING", 1, true, [ false ]] """)
  }

  "StringValue" - {
    "StringValue.toStringValue" in {
      assert(StringValue("X").toStringValue == Right(StringValue("X")))
    }

    "StringValue.toNumeric" in {
      assert(StringValue("X").toNumeric.isLeft)
      assert(StringValue("0").toNumeric == Right(NumericValue(0)))
      assert(StringValue("009").toNumeric == Right(NumericValue(9)))
      assert(StringValue(bigNumber).toNumeric == Right(NumericValue(BigDecimal(bigNumber))))
    }

    "StringValue.toBoolean" in {
      assert(StringValue("X").toBoolean.isLeft)

      assert(StringValue("true").toBoolean == Right(BooleanValue(true)))
      assert(StringValue("false").toBoolean == Right(BooleanValue(false)))
    }
  }

  "NumericValue" - {
    "NumericValue.toStringValue" in {
      assert(NumericValue(7).toStringValue == Right(StringValue("7")))
      assert(NumericValue(BigDecimal("1.2e99")).toStringValue == Right(StringValue("1.2E+99")))
      assert(NumericValue(BigDecimal(bigNumber)).toStringValue ==
        Right(StringValue("1.112223334445556667778889999998887776665555444333222111E+125")))
    }

    "NumericValue.toNumeric" in {
      assert(NumericValue(7).toNumeric == Right(NumericValue(7)))
    }

    "NumericValue.toBoolean" in {
      assert(NumericValue(1).toBoolean == Right(BooleanValue(true)))
      assert(NumericValue(0).toBoolean == Right(BooleanValue(false)))
      assert(NumericValue(-1).toBoolean.isLeft)
    }
  }

  "BooleanValue" - {
    "BooleanValue.toStringValue" in {
      assert(BooleanValue(true).toStringValue == Right(StringValue("true")))
      assert(BooleanValue(false).toStringValue == Right(StringValue("false")))
    }

    "BooleanValue.toNumeric" in {
      assert(BooleanValue(true).toNumeric == Right(NumericValue(1)))
      assert(BooleanValue(false).toNumeric == Right(NumericValue(0)))
    }

    "BooleanValue.toBoolean" in {
      assert(BooleanValue(true).toBoolean == Right(BooleanValue(true)))
      assert(BooleanValue(false).toBoolean == Right(BooleanValue(false)))
    }
  }
}
