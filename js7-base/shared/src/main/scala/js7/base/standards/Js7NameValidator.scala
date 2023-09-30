package js7.base.standards

import java.lang.Character.isSurrogate
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.standards.Js7NameValidator.*

final class Js7NameValidator(val typeName: String)
extends NameValidator:

  private val javaNameValidator = new JavaNameValidator(
    typeName,
    isExtraNameStart = _.isDigit,
    isExtraNamePart = c => isExtraNamePart0(c) || isSurrogate(c))

  override def checked(name: String) =
    if name endsWith "." then
      Left(InvalidNameProblem(typeName, name))
    else
      javaNameValidator.checked(name)

  def isNameStart(c: Char) =
    javaNameValidator.isNameStart(c)

  def isNamePart(c: Char) =
    javaNameValidator.isNamePart(c) || isExtraNamePart0(c)

object Js7NameValidator:
  private val isExtraNamePart0 = Set('-', '.')
