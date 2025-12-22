package js7.base.auth

import java.lang.Character.{isIdentifierIgnorable, isUnicodeIdentifierPart, isUnicodeIdentifierStart}
import js7.base.generic.GenericString
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidNameProblem
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
final case class UserId private(string: String) extends GenericString:

  def isAnonymous: Boolean = this == UserId.Anonymous

  override def toString = s"User:$string"


object UserId extends GenericString.Checked_[UserId]:
  val Anonymous: UserId = UserId("Anonymous")

  @TestOnly
  val Test: UserId = UserId("TEST")

  def unchecked(string: String): UserId =
    new UserId(string)

  override def checked(string: String): Checked[UserId] =
    if string.isEmpty then
      Left(EmptyStringProblem(name))
    else if isValid(string) then
      Right(new UserId(string))
    else
      Left(InvalidNameProblem(name, string))

  private def isValid(string: String): Boolean =
    string.nonEmpty &&
      string.last != '-' &&
      !string.contains("--") &&
      isNameStart(string.charAt(0)) &&
      1.until(string.length).forall(i => isNamePart(string.charAt(i)))

  private def isNameStart(c: Char): Boolean =
    isUnicodeIdentifierStart(c) || c.isDigit

  private def isNamePart(c: Char): Boolean =
    isUnicodeIdentifierPart(c) && !isIdentifierIgnorable(c) || c == '-' || c == '.'
