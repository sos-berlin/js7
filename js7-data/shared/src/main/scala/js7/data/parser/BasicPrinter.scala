package js7.data.parser

import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}

object BasicPrinter
{
  def appendIdentifier(sb: StringBuilder, identifier: String): Unit =
    if (identifierRequiresBacktick(identifier)) {
      appendIdentifierWithBackticks(sb, identifier)
    } else {
      sb.append(identifier)
    }

  def appendIdentifierWithBackticks(sb: StringBuilder, identifier: String): Unit = {
      sb.append('`')
      sb.append(identifier.replace("`", "``"))
      sb.append('`')
  }

  def identifierRequiresBacktick(identifier: String) =
    !isIdentifier(identifier) && !identifier.forall(isDigit)

  def isIdentifier(string: String) =
    string.nonEmpty &&
      string.last != '-' &&
      isIdentifierStart(string.charAt(0)) &&
      (1 until string.length).forall(i => isIdentifierPart(string.charAt(i)))

  def isIdentifierStart(c: Char) =
    isUnicodeIdentifierStart(c) /*|| isHighSurrogate(c)*/

  def isIdentifierPart(c: Char) =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) /*|| isSurrogate(c)*/

  private def isDigit(c: Char) =
    c >= '0' && c <= '9'
}
