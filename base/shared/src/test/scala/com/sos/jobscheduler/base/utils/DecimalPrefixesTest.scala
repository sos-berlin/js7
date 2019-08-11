package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.DecimalPrefixes.{toInt, toLong}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class DecimalPrefixesTest extends FreeSpec
{
  "toInt" in {
    assert(toInt("") == Left(Problem("NumberFormatException: For input string: \"\"")))
    assert(toInt("k") == Left(Problem("NumberFormatException: For input string: \"\"")))
    assert(toInt("xk") == Left(Problem("NumberFormatException: For input string: \"x\"")))
    assert(toInt("1K") == Left(Problem("Unknown SI prefix: 'K', expected one of k, M, G")))
    assert(toInt("1") == Right(1))
    assert(toInt("1k") == Right(1000))
    assert(toInt("-1k") == Right(-1000))
    assert(toInt("12k") == Right(12000))
    assert(toInt("1M") == Right(1000*1000))
    assert(toInt("1G") == Right(1000*1000*1000))
  }

  "toLong" in {
    assert(toLong("1000G") == Right(1000L*1000*1000*1000))
  }
}
