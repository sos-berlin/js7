package js7.base.standards

import java.lang.Character.{isHighSurrogate, isIdentifierIgnorable, isSurrogate, isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidNameProblem

final class JavaNameValidator(
  val typeName: String,
  val isExtraNameStart: Char => Boolean = _ => false,
  val isExtraNamePart: Char => Boolean = _ => false)
extends NameValidator
{
  def checked(name: String): Checked[String] =
    if name.isEmpty then
      Left(EmptyStringProblem(typeName))
    else if isValid(name) then
      Right(name)
    else
      Left(InvalidNameProblem(typeName, name))

  def isValid(string: String): Boolean =
    string.nonEmpty &&
      isNameStart(string(0)) &&
      (1 until string.length).forall(i => isNamePart(string.apply(i)))

  def isNameStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c) || isHighSurrogate(c) ||
      isExtraNameStart(c)

  def isNamePart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) ||
      isSurrogate(c)/*Like 🥕*/ ||
      isExtraNamePart(c)
}
