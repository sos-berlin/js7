package com.sos.jobscheduler.base.utils

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}

final case class Identifier private(string: String) extends IsString

/**
  * @author Joacim Zschimmer
  */
object Identifier
{
  def checked(string: String): Checked[Identifier] =
    if (isIdentifier(string))
      Valid(new Identifier(string))
    else
      Problem(s"Invalid character or character combination in identifier '$string'")

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
    isUnicodeIdentifierStart(c) || character.isHighSurrogate(c)

  def isIdentifierPart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || c == '-' || character.isSurrogate(c)

  private object character { // Corresponding Java methods are not implemented in Scala.js
    def isHighSurrogate(c: Char) = c >= '\uD800' && c <= '\uDBFF'
    def isSurrogate(c: Char)     = c >= '\uD800' && c <= '\uDFFF'
  }
}
