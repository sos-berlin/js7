package com.sos.jobscheduler.base.utils

import scala.math.max

/**
  * @author Joacim Zschimmer
  */
object Strings {

  private val Ellipsis = "..."
  private val CharCountThreshold = 50  // truncateWithEllipsis adds character count if string is not shorter

  implicit class TruncatedString(val underlying: String) extends AnyVal {
    /** Truncate to `n`, replacing the tail with ellipsis and, if the string is long, the total character count. */
    def truncateWithEllipsis(n: Int): String =
      truncateWithEllipsis(n, showLength = n >= CharCountThreshold)

    /** Truncate to `n`, replacing the tail with ellipsis and, if the string is long, the total character count. */
    def truncateWithEllipsis(n: Int, showLength: Boolean): String = {
      val suffix = if (showLength) s"$Ellipsis(length ${underlying.length})" else Ellipsis
      val nn = max(suffix.length, n)
      if (underlying.length <= nn)
        underlying
      else
        underlying.take(nn - suffix.length) + suffix
    }
  }
}
