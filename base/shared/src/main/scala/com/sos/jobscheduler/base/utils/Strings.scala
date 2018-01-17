package com.sos.jobscheduler.base.utils

import scala.math.max

/**
  * @author Joacim Zschimmer
  */
object Strings {

  private val Ellipsis = "..."

  implicit class TruncatedString(val underlying: String) extends AnyVal {
    ///** Truncate to `n`, replacing the tail with ellipsis and, if the string is long, the total character count. */
    def truncateWithEllipsis(n: Int): String =
      truncateWithEllipsis(n, showLength = false)

    /** Truncate to `n`, replacing the tail with ellipsis and, if the string is long, the total character count. */
    def truncateWithEllipsis(n: Int, showLength: Boolean = false): String = {
      val suffix = if (showLength) s"$Ellipsis(length ${underlying.length})" else Ellipsis
      val nn = max(suffix.length, n)
      if (underlying.length <= nn)
        underlying
      else
        underlying.take(nn - suffix.length) + suffix
    }
  }
}
