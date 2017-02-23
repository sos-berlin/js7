package com.sos.scheduler.engine.common.utils

import scala.math.max

/**
  * @author Joacim Zschimmer
  */
object Strings {

  private val Ellipsis = "..."

  implicit class TruncatedString(val delegate: String) extends AnyVal {
    def truncateWithEllipsis(n: Int): String = {
      val nn = max(Ellipsis.length, n)
      if (delegate.length <= nn)
        delegate
      else
        delegate.take(nn - Ellipsis.length) + "..."
    }
  }
}
