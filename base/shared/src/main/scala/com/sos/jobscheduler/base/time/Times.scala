package com.sos.jobscheduler.base.time

import scala.concurrent.duration._

object Times
{
  implicit final class RichFiniteDuration(private val underlying: FiniteDuration) extends AnyVal
  {
    def toBigDecimalSeconds = underlying.unit match {
      case NANOSECONDS  => BigDecimal(underlying.length, 9)
      case MICROSECONDS => BigDecimal(underlying.length, 6)
      case MILLISECONDS => BigDecimal(underlying.length, 3)
      case SECONDS      => BigDecimal(underlying.length, 0)
      case MINUTES      => BigDecimal(underlying.length) * 60
      case HOURS        => BigDecimal(underlying.length) * (60 * 60)
      case DAYS         => BigDecimal(underlying.length) * (60 * 60 * 24)
    }
  }
}
