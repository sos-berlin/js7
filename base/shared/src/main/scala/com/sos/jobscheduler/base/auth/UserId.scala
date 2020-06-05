package js7.base.auth

import js7.base.generic.GenericString
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problems.InvalidNameProblem
import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}

/**
  * @author Joacim Zschimmer
  */
final case class UserId private(string: String) extends GenericString
{
  def isAnonymous = this == UserId.Anonymous
}

object UserId extends GenericString.Checked_[UserId]
{
  val Anonymous = UserId("Anonymous")

  def unchecked(string: String) = new UserId(string)

  override def checked(string: String) =
    if (string.isEmpty)
      Left(EmptyStringProblem(name))
    else if (isValid(string))
      Right(new UserId(string))
    else
      Left(InvalidNameProblem(name, string))

  private def isValid(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      !string.contains("--") &&
      isNameStart(string charAt 0) &&
      (1 until string.length forall { i => isNamePart(string charAt i) })

  private def isNameStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c) || c.isDigit

  private def isNamePart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || c == '-' || c == '.'
}
