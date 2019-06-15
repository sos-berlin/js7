package com.sos.jobscheduler.base.utils

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.DecimalPrefixes.{toInt, toLong}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class DecimalPrefixesTest extends FreeSpec
{
  "toInt" in {
    assert(toInt("") == Invalid(Problem("NumberFormatException: For input string: \"\"")))
    assert(toInt("k") == Invalid(Problem("NumberFormatException: For input string: \"\"")))
    assert(toInt("xk") == Invalid(Problem("NumberFormatException: For input string: \"x\"")))
    assert(toInt("1K") == Invalid(Problem("Unknown SI prefix: 'K', expected one of k, M, G")))
    assert(toInt("1") == Valid(1))
    assert(toInt("1k") == Valid(1000))
    assert(toInt("-1k") == Valid(-1000))
    assert(toInt("12k") == Valid(12000))
    assert(toInt("1M") == Valid(1000*1000))
    assert(toInt("1G") == Valid(1000*1000*1000))
  }

  "toLong" in {
    assert(toLong("1000G") == Valid(1000L*1000*1000*1000))
  }
}
