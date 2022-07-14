package js7.base.convert

import js7.base.convert.As.{StringAsIntOrUnlimited, StringAsPercentage}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class AsTest extends AnyFreeSpec {

  "StringAsInt" in {
    val conv = implicitly[As[String, Int]]
    intercept[NumberFormatException] { conv("") }
    intercept[NumberFormatException] { conv(" 1") }
    intercept[NumberFormatException] { conv("1 ") }
    intercept[NumberFormatException] { conv("1x") }
    assert(conv("123") == 123)
    assert(conv("+123") == 123)
    assert(conv("-123") == -123)
    assert(conv("2147483647") == Int.MaxValue)
    intercept[NumberFormatException] { conv("2147483648") }
    assert(conv("-2147483648") == Int.MinValue)
    intercept[NumberFormatException] { conv("-2147483649") }
  }

  "StringAsIntOrUnlimited" in {
    intercept[NumberFormatException] { StringAsIntOrUnlimited("x") }
    assert(StringAsIntOrUnlimited("unlimited") == None)
    assert(StringAsIntOrUnlimited("123") == Some(123))
    assert(StringAsIntOrUnlimited("2147483647") == Some(Int.MaxValue))
    intercept[NumberFormatException] { StringAsIntOrUnlimited("2147483648") }
  }

  "StringAsBigDecimal" in {
    val conv = implicitly[As[String, BigDecimal]]
    intercept[NumberFormatException] { conv("") }
    intercept[NumberFormatException] { conv(" 1") }
    intercept[NumberFormatException] { conv("1 ") }
    intercept[NumberFormatException] { conv("1x") }
    assert(conv("123") == BigDecimal(123))
    assert(conv("123.1") == BigDecimal("123.1"))
    assert(conv("+123") == BigDecimal(123))
    assert(conv("-123") == BigDecimal(-123))
    intercept[NumberFormatException] { conv("1,0") }
  }

  "StringAsBoolean" in {
    val conv = implicitly[As[String, Boolean]]
    intercept[IllegalArgumentException] { conv("") }
    intercept[IllegalArgumentException] { conv("1") } .getMessage shouldEqual "Boolean value true or false expected, not: 1"
    assert(conv("true"))
    assert(conv("on"))
    assert(conv("yes"))
    assert(!conv("false"))
    assert(!conv("off"))
    assert(!conv("no"))
    for (o <- List(true, false)) assert(conv(o.toString) == o)
  }

  "StringAsPercentage" in {
    intercept[IllegalArgumentException] { StringAsPercentage("") }
    assert(StringAsPercentage("1") == BigDecimal("1.0"))
    assert(StringAsPercentage("1%") == BigDecimal("0.01"))
    assert(StringAsPercentage("-1%") == BigDecimal("-0.01"))
    assert(StringAsPercentage("100") == BigDecimal("100"))
    assert(StringAsPercentage("100%") == BigDecimal("1.0"))
  }
}
