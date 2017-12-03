package com.sos.jobscheduler.base.utils

import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}
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
