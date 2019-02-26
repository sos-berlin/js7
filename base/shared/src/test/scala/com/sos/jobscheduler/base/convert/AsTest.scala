package com.sos.jobscheduler.base.convert

import com.sos.jobscheduler.base.convert.As.StringAsIntOrUnlimited
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class AsTest extends FreeSpec {

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
}
