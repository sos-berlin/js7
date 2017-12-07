package com.sos.jobscheduler.base.utils

import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}
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

  def requireExtendedIdentifier(string: String): String = {
    if (!isExtendedIdentifier(string)) throw new IllegalArgumentException(s"Not a valid identifier: $string")
    string
  }

  /**
    * Like a Java identifier with minus character.
    * Minus character is not allowed as last or first character.
    */
  def isExtendedIdentifier(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      isUnicodeIdentifierStart(string charAt 0) &&
      (1 until string.length forall { i ⇒
        val c = string charAt i
        isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || c == '-'
      })
}
