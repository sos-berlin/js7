package com.sos.jobscheduler.base.generic

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.GenericString.EmptyStringProblem
import com.sos.jobscheduler.base.generic.GenericStringTest._
import com.sos.jobscheduler.base.problem.Problems.InvalidNameProblem
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericStringTest extends FreeSpec
{
  "NonEmpty.checked" in {
    assert(NonEmptyA.checked("") == Invalid(EmptyStringProblem("NonEmptyA")))
  }

  "NameValidating.checked" in {
    assert(ValidatedA.checked("validated") == Valid(ValidatedA("validated")))
    assert(ValidatedA.checked("") == Invalid(EmptyStringProblem("ValidatedA")))
    assert(ValidatedA.checked("/") == Invalid(InvalidNameProblem(typeName = "ValidatedA", name = "/")))
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
