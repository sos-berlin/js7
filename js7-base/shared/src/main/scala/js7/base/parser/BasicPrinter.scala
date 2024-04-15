package js7.base.parser

import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.utils.ScalaUtils.withStringBuilder

object BasicPrinter:

  def identifierToString(identifier: String): String =
    if identifierRequiresBacktick(identifier) then
      withStringBuilder(appendIdentifierWithBackticks(_, identifier))
    else
      identifier

  def appendIdentifier(sb: StringBuilder, identifier: String): Unit =
    if identifierRequiresBacktick(identifier) then
      appendIdentifierWithBackticks(sb, identifier)
    else
      sb.append(identifier)

  def appendIdentifierWithBackticks(sb: StringBuilder, identifier: String): Unit =
      sb.append('`')
      sb.append(identifier.replace("`", "``"))
      sb.append('`')

  def identifierRequiresBacktick(identifier: String): Boolean =
    !isIdentifier(identifier) && !identifier.forall(isDigit)

  def isIdentifier(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      isIdentifierStart(string.charAt(0)) &&
      (1 until string.length).forall(i => isIdentifierPart(string.charAt(i)))

  def isIdentifierStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c) /*|| isHighSurrogate(c)*/

  def isIdentifierPart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) /*|| isSurrogate(c)*/

  private def isDigit(c: Char) =
    c >= '0' && c <= '9'
