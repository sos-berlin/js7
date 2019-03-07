package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.time.Times._
import java.util.concurrent.TimeUnit.{DAYS, HOURS, MICROSECONDS, MILLISECONDS, MINUTES, NANOSECONDS, SECONDS}
import org.scalatest.FreeSpec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final class TimesTest extends FreeSpec
{
  "FiniteDuration toBigDecimalSeconds" in {
    assert(FiniteDuration(1234, NANOSECONDS).toBigDecimalSeconds == BigDecimal("0.000001234"))
    assert(FiniteDuration(-1234, NANOSECONDS).toBigDecimalSeconds == BigDecimal("-0.000001234"))
    assert(FiniteDuration(1234, MICROSECONDS).toBigDecimalSeconds == BigDecimal("0.001234"))
    assert(FiniteDuration(-1234, MICROSECONDS).toBigDecimalSeconds == BigDecimal("-0.001234"))
    assert(FiniteDuration(1234, MILLISECONDS).toBigDecimalSeconds == BigDecimal("1.234"))
    assert(FiniteDuration(-1234, MILLISECONDS).toBigDecimalSeconds == BigDecimal("-1.234"))
    assert(FiniteDuration(1234, SECONDS).toBigDecimalSeconds == BigDecimal("1234"))
    assert(FiniteDuration(-1234, SECONDS).toBigDecimalSeconds == BigDecimal("-1234"))
    assert(FiniteDuration(1, MINUTES).toBigDecimalSeconds == BigDecimal("60"))
    assert(FiniteDuration(-1, MINUTES).toBigDecimalSeconds == BigDecimal("-60"))
    assert(FiniteDuration(1, HOURS).toBigDecimalSeconds == BigDecimal("3600"))
    assert(FiniteDuration(-1, HOURS).toBigDecimalSeconds == BigDecimal("-3600"))
    assert(FiniteDuration(1, DAYS).toBigDecimalSeconds == BigDecimal("86400"))
    assert(FiniteDuration(-1, DAYS).toBigDecimalSeconds == BigDecimal("-86400"))

    assert(FiniteDuration(Long.MaxValue, NANOSECONDS).toBigDecimalSeconds == BigDecimal("9223372036.854775807"))
  }
}
