package js7.base.standards

import java.lang.Character.{isHighSurrogate, isIdentifierIgnorable, isSurrogate, isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidNameProblem

/**
  * @author Joacim Zschimmer
  */
class NameValidator(isAllowedChar: Char => Boolean = _ => false)
{
  final def checked(typeName: String, name: String): Checked[String] =
    if (name.isEmpty)
      Left(EmptyStringProblem(typeName))
    else if (isValid(name))
      Right(name)
    else
      Left(InvalidNameProblem(typeName, name))

  final def isValid(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      isNameStart(string charAt 0) &&
      (1 until string.length forall { i => isNamePart(string charAt i) })

  final def isNameStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c) || isHighSurrogate(c)

  final def isNamePart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || isSurrogate(c) || isAllowedChar(c)
}

object NameValidator extends NameValidator(Set('-', '.'))
