package com.sos.scheduler.engine.common.convert

import com.sos.scheduler.engine.common.convert.Converters._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ConvertersTest extends FreeSpec {

  "StringToInt" in {
    val conv = implicitly[To[String, Int]]
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

  "StringToBoolean" in {
    val conv = implicitly[To[String, Boolean]]
    intercept[IllegalArgumentException] { conv("") }
    intercept[IllegalArgumentException] { conv("1") } .getMessage shouldEqual "Boolean value true or false expected, not: 1"
    assert(conv("true"))
    assert(conv("on"))
    assert(conv("yes"))
    assert(!conv("false"))
    assert(!conv("off"))
    assert(!conv("no"))
    for (o ← List(true, false)) assert(conv(o.toString) == o)
  }
}
