package com.sos.jobscheduler.data.filebased

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import java.lang.Character.{isHighSurrogate, isIdentifierIgnorable, isSurrogate, isUnicodeIdentifierPart, isUnicodeIdentifierStart}

/**
  * @author Joacim Zschimmer
  */
class NameValidator(isAllowedChar: Char ⇒ Boolean = _ ⇒ false)
{
  final def checked(string: String): Checked[String] =
    if (isValid(string))
      Valid(string)
    else
      Problem(s"Invalid character or character combination in name '$string'")

  final def isValid(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      isNameStart(string charAt 0) &&
      (1 until string.length forall { i ⇒ isNamePart(string charAt i) })

  final def isNameStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c) || isHighSurrogate(c)

  final def isNamePart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || isSurrogate(c) || isAllowedChar(c)
}

object NameValidator extends NameValidator(Set('-', '.'))
