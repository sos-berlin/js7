package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.DecimalPrefixes.toInt
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class DecimalPrefixesTest extends FreeSpec
{
  "toInt" in {
    assert(intercept[Exception] { toInt("") }.toString == "java.lang.NumberFormatException: For input string: \"\"")
    assert(intercept[Exception] { toInt("k") }.toString == "java.lang.NumberFormatException: For input string: \"\"")
    assert(intercept[Exception] { toInt("1K") }.getMessage == "Unknown SI prefix: 'K', expected: k, M, G")
    assert(toInt("1") == 1)
    assert(toInt("1k") == 1000)
    assert(toInt("-1k") == -1000)
    assert(toInt("12k") == 12000)
    assert(toInt("1M") == 1000*1000)
    assert(toInt("1G") == 1000*1000*1000)
  }
}
