package js7.base.utils

import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}

final case class Identifier private(string: String) extends GenericString

/**
  * @author Joacim Zschimmer
  */
object Identifier
{
  def checked(string: String): Checked[Identifier] =
    if (isIdentifier(string))
      Right(new Identifier(string))
    else
      Problem(s"Invalid character or character combination in identifier: $string")

  // TODO Check syntax and match with ExpressionParser
  /**
    * Like a Java identifier with minus character and UNICODE surrogates.
    * Minus character is not allowed as last or first character.
    */
  def isIdentifier(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      isIdentifierStart(string.charAt(0)) &&
      (1 until string.length forall { i => isIdentifierPart(string.charAt(i)) })

  def isIdentifierStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c) || isHighSurrogate(c)

  def isIdentifierPart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || c == '-' || isSurrogate(c)

  // Corresponding Java methods are not implemented in Scala.js
  private def isHighSurrogate(c: Char) = c >= '\uD800' && c <= '\uDBFF'
  private def isSurrogate(c: Char)     = c >= '\uD800' && c <= '\uDFFF'
}
