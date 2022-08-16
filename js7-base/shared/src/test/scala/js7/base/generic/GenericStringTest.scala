package js7.base.generic

import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.generic.GenericStringTest.*
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class GenericStringTest extends Test
{
  "NonEmpty.checked" in {
    assert(NonEmptyA.checked("") == Left(EmptyStringProblem("NonEmptyA")))
  }

  "NameValidating.checked" in {
    assert(ValidatedA.checked("validated") == Right(ValidatedA("validated")))
    assert(ValidatedA.checked("") == Left(EmptyStringProblem("ValidatedA")))
    assert(ValidatedA.checked("/") == Left(InvalidNameProblem(typeName = "ValidatedA", name = "/")))
  }
}

private object GenericStringTest
{
  private case class ValidatedA(string: String) extends GenericString
  private object ValidatedA extends GenericString.NameValidating[ValidatedA] {
    protected def unchecked(string: String) = new ValidatedA(string)
  }

  private case class NonEmptyA(string: String) extends GenericString
  private object NonEmptyA extends GenericString.NameValidating[NonEmptyA] {
    protected def unchecked(string: String) = new NonEmptyA(string)
  }
}
