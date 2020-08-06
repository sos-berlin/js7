package js7.base.utils

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.instances.int._
import cats.instances.string._
import js7.base.problem.{Problem, ProblemException}
import js7.base.utils.CatsUtils._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CatsUtilsTest extends AnyFreeSpec
{
  "combineArgs" in {
    assert(combineArgs(1, 2, 3, 4) == 10)
    assert(combineArgs("a", "-", "b") == "a-b")
  }

  "Validated[Throwable, _] orThrow" in {
    assert((Valid(1): Validated[Throwable, Int]).orThrow == 1)

    val invalid: Validated[Throwable, Int] = Invalid(new IllegalArgumentException("THROWABLE"))
    assert(intercept[IllegalArgumentException](invalid.orThrow).getMessage == "THROWABLE")
  }

  "Validated[Problem, _] orThrow" in {
    assert((Valid(1): Validated[Problem, Int]).orThrow == 1)

    val invalid: Validated[Problem, Int] = Invalid(Problem("PROBLEM"))
    assert(intercept[ProblemException](invalid.orThrow).getMessage == "PROBLEM")
  }
}
