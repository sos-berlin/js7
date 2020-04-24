package com.sos.jobscheduler.base.utils

import scala.math.max

/**
  * @author Joacim Zschimmer
  */
object Strings
{
  private val Ellipsis = "..."

  implicit final class RichString(private val underlying: String) extends AnyVal
  {
    /** Truncate to `n`, replacing the tail with ellipsis and, if the string is long, the total character count. */
    def truncateWithEllipsis(n: Int, showLength: Boolean = false): String = {
      val suffix = if (showLength) s"$Ellipsis(length ${underlying.length})" else Ellipsis
      val nn = max(suffix.length, n)
      if (underlying.lengthIs <= nn)
        underlying
      else
        underlying.take(nn - suffix.length) + suffix
    }

    def replaceChar(from: Char, to: Char): String =
      if (underlying contains from) {
        val chars = new Array[Char](underlying.length)
        underlying.getChars(0, underlying.length, chars, 0)
        for (i <- chars.indices) if (chars(i) == from) chars(i) = to
        new String(chars)
      } else
        underlying

    def reverseDropWhile(predicate: Char => Boolean): String = {
      var i = underlying.length
      while (i > 0 && predicate(underlying(i - 1))) i = i -1
      underlying.substring(0, i)
    }

    def ?:(condition: Boolean): String =
      when(condition)

    @inline def when(condition: Boolean): String =
      if (condition) underlying else ""
  }
}
