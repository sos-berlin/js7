package com.sos.jobscheduler.base.utils

import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}

/**
  * @author Joacim Zschimmer
  */
object Identifier {
  def requireIdentifier(string: String): String = {
    if (!isIdentifier(string)) throw new IllegalArgumentException(s"Not a valid identifier: $string")
    string
  }

  /**
    * Like a Java identifier with minus character.
    * Minus character is not allowed as last or first character.
    */
  def isIdentifier(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      isIdentifierStart(string charAt 0) &&
      (1 until string.length forall { i ⇒ isIdentifierPart(string charAt i) })

  def isIdentifierStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c)

  def isIdentifierPart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || c == '-'
}
